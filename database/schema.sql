-- AI Health Atlas Database Schema (SQLite)

-- 1. Districts
CREATE TABLE IF NOT EXISTS districts (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    name TEXT UNIQUE NOT NULL,
    area_sqkm REAL NOT NULL,
    description TEXT
);

-- 2. Demographics (Individual resident level, 50,000+ rows)
CREATE TABLE IF NOT EXISTS demographics (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    district_id INTEGER NOT NULL,
    year INTEGER NOT NULL,
    age INTEGER NOT NULL,
    gender TEXT NOT NULL,         -- 'Male', 'Female'
    income REAL NOT NULL,         -- Annual income in EUR
    education TEXT NOT NULL,      -- 'Primary', 'Secondary', 'Tertiary'
    employed INTEGER NOT NULL,    -- 0 = Unemployed, 1 = Employed
    life_expectancy REAL NOT NULL, -- Estimated life expectancy
    FOREIGN KEY(district_id) REFERENCES districts(id) ON DELETE CASCADE
);

-- 3. Environmental Quality (Daily or monthly district-level logs, 20,000+ rows)
CREATE TABLE IF NOT EXISTS environmental (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    district_id INTEGER NOT NULL,
    date TEXT NOT NULL,               -- 'YYYY-MM-DD'
    pm25 REAL NOT NULL,               -- PM2.5 in ug/m3
    temperature REAL NOT NULL,        -- Avg temperature in Celsius
    green_space_pct REAL NOT NULL,    -- Percentage of green space (district constant or seasonal)
    noise_level_db REAL NOT NULL,     -- Decibels
    water_quality_index REAL NOT NULL, -- Score from 0 to 100
    FOREIGN KEY(district_id) REFERENCES districts(id) ON DELETE CASCADE
);

-- 4. Healthcare Resources (Locations, beds, doctors, response times)
CREATE TABLE IF NOT EXISTS healthcare_resources (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    district_id INTEGER NOT NULL,
    name TEXT NOT NULL,
    type TEXT NOT NULL,               -- 'Hospital', 'Clinic', 'Specialist'
    latitude REAL NOT NULL,
    longitude REAL NOT NULL,
    beds INTEGER NOT NULL,
    doctors INTEGER NOT NULL,
    emergency_response_time_min REAL NOT NULL,
    FOREIGN KEY(district_id) REFERENCES districts(id) ON DELETE CASCADE
);

-- 5. Epidemiology (Chronic conditions & lifestyle metrics for residents, 50,000+ rows)
CREATE TABLE IF NOT EXISTS epidemiology (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    demographic_id INTEGER NOT NULL UNIQUE,
    diabetes INTEGER NOT NULL,        -- 0 = No, 1 = Yes
    heart_disease INTEGER NOT NULL,   -- 0 = No, 1 = Yes
    obesity INTEGER NOT NULL,         -- 0 = No, 1 = Yes
    smoking INTEGER NOT NULL,         -- 0 = No, 1 = Yes
    vaccinated INTEGER NOT NULL,      -- 0 = No, 1 = Yes
    mental_health_index REAL NOT NULL, -- Score from 0 to 100
    FOREIGN KEY(demographic_id) REFERENCES demographics(id) ON DELETE CASCADE
);

-- 6. User Authentication Table
CREATE TABLE IF NOT EXISTS users (
    id INTEGER PRIMARY KEY AUTOINCREMENT,
    username TEXT UNIQUE NOT NULL,
    password_hash TEXT NOT NULL,
    role TEXT NOT NULL CHECK(role IN ('Admin', 'Researcher', 'Viewer')),
    full_name TEXT,
    created_at TIMESTAMP DEFAULT CURRENT_TIMESTAMP
);

-- Performance Indexes
CREATE INDEX IF NOT EXISTS idx_demographics_district_year ON demographics (district_id, year);
CREATE INDEX IF NOT EXISTS idx_demographics_age_gender ON demographics (age, gender);
CREATE INDEX IF NOT EXISTS idx_epidemiology_demographic ON epidemiology (demographic_id);
CREATE INDEX IF NOT EXISTS idx_environmental_district_date ON environmental (district_id, date);
CREATE INDEX IF NOT EXISTS idx_healthcare_district_type ON healthcare_resources (district_id, type);
