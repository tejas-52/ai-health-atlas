import os
import sqlite3
import random
import hashlib
import csv
from datetime import datetime, timedelta
import math

# Set random seed for reproducibility
random.seed(42)

DB_PATH = os.path.join("database", "health_atlas.db")
SCHEMA_PATH = os.path.join("database", "schema.sql")
RAW_DATA_DIR = os.path.join("data", "raw")
PROCESSED_DATA_DIR = os.path.join("data", "processed")

# Ensure directories exist
os.makedirs("database", exist_ok=True)
os.makedirs(RAW_DATA_DIR, exist_ok=True)
os.makedirs(PROCESSED_DATA_DIR, exist_ok=True)

def hash_password(password):
    return hashlib.sha256(password.encode('utf-8')).hexdigest()

def create_database():
    print(f"Creating database at: {DB_PATH} using schema: {SCHEMA_PATH}")
    if os.path.exists(DB_PATH):
        try:
            os.remove(DB_PATH)
        except Exception as e:
            print(f"Warning: Could not remove existing database file: {e}")
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    
    with open(SCHEMA_PATH, 'r') as f:
        schema_sql = f.read()
    
    cursor.executescript(schema_sql)
    conn.commit()
    conn.close()

def generate_and_seed_data():
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    
    # 1. Seed Districts
    districts_data = [
        # (name, area_sqkm, description, baseline_income, baseline_life_exp, green_space, noise)
        ("Altstadt", 17.5, "Historic center, high density, commercial hub.", 38000, 79.5, 12.0, 68),
        ("Neustadt", 14.8, "Trendy urban quarter, young demographics, nightlife.", 32000, 78.0, 15.0, 72),
        ("Pieschen", 16.2, "Residential district, family-friendly, gentrifying.", 34000, 79.0, 18.0, 64),
        ("Klotzsche", 27.1, "Suburban area, airport location, high green space.", 48000, 81.5, 45.0, 52),
        ("Loschwitz", 29.8, "Hillside wealthy district, oldest average age, forests.", 58000, 83.5, 50.0, 48),
        ("Blasewitz", 15.6, "Affluent riverside villas, older age distribution.", 52000, 82.0, 32.0, 55),
        ("Plauen", 22.4, "Mixed residential and university area.", 40000, 80.5, 25.0, 58),
        ("Cotta", 19.3, "Working-class neighborhood, higher density.", 29000, 77.0, 14.0, 66),
        ("Prohlis", 21.1, "High-density concrete estates, socio-economically challenged.", 23000, 74.5, 20.0, 65),
        ("Leuben", 13.5, "Older residential area, medium density.", 28000, 76.5, 22.0, 60)
    ]
    
    print("Seeding districts...")
    cursor.executemany(
        "INSERT INTO districts (name, area_sqkm, description) VALUES (?, ?, ?)",
        [(d[0], d[1], d[2]) for d in districts_data]
    )
    conn.commit()
    
    # Fetch district IDs for mapping
    cursor.execute("SELECT id, name FROM districts")
    district_map = {name: id for id, name in cursor.fetchall()}
    
    # 2. Seed Users
    print("Seeding users...")
    users_data = [
        ("admin", hash_password("admin123"), "Admin", "System Administrator"),
        ("researcher", hash_password("research123"), "Researcher", "Lead Health Analyst"),
        ("viewer", hash_password("view123"), "Viewer", "Public Health Observer")
    ]
    cursor.executemany(
        "INSERT INTO users (username, password_hash, role, full_name) VALUES (?, ?, ?, ?)",
        users_data
    )
    conn.commit()

    # 3. Seed Healthcare Resources
    print("Seeding healthcare resources...")
    # Generate around 30 medical facilities in Dresden area
    healthcare_facilities = []
    # Dresden central lat/lon: 51.0504, 13.7373
    facility_types = ["Hospital", "Clinic", "Specialist"]
    
    facility_templates = [
        ("Dresden Friedrichstadt Hospital", "Hospital", 51.0583, 13.7188, 650, 120, 8.5, "Altstadt"),
        ("Dresden Neustadt Clinic", "Clinic", 51.0822, 13.7255, 120, 35, 12.0, "Neustadt"),
        ("Klotzsche Medical Center", "Specialist", 51.1154, 13.7744, 15, 18, 14.5, "Klotzsche"),
        ("Loschwitz Cardiovascular Clinic", "Hospital", 51.0575, 13.8188, 250, 60, 11.0, "Loschwitz"),
        ("Prohlis Community Health Hub", "Clinic", 51.0088, 13.7911, 45, 20, 9.0, "Prohlis"),
        ("Cotta Urgent Care", "Clinic", 51.0544, 13.6822, 35, 15, 10.5, "Cotta"),
        ("Blasewitz Pediatrics", "Specialist", 51.0522, 13.8011, 0, 12, 15.0, "Blasewitz"),
        ("Pieschen General Hospital", "Hospital", 51.0788, 13.7144, 400, 85, 10.0, "Pieschen"),
        ("Plauen Family Health Clinic", "Clinic", 51.0266, 13.7199, 20, 14, 11.5, "Plauen"),
        ("Leuben Geriatric Center", "Hospital", 51.0111, 13.8244, 180, 40, 13.0, "Leuben")
    ]
    
    # Insert template facilities
    for name, ftype, lat, lon, beds, docs, resp, dist_name in facility_templates:
        dist_id = district_map[dist_name]
        healthcare_facilities.append((dist_id, name, ftype, lat, lon, beds, docs, resp))
        
    # Generate some extra random clinics and specialist centers
    facility_names = [
        "Eye Care Specialists", "Dermatology Center Dresden", "Neustadt Children Clinic", 
        "Altstadt Dental Care", "Plauen Physiotherapy", "Cotta Mental Health Center",
        "Dresden South Hospital", "Elbe Valley Medical Practice", "Heide Park Clinic",
        "Dresden East Medical Center", "Orthopedic Specialist Center", "Pulmonology Dresden"
    ]
    for i, name in enumerate(facility_names):
        dist_name = districts_data[i % len(districts_data)][0]
        dist_id = district_map[dist_name]
        ftype = random.choice(facility_types)
        # Random perturbation around Dresden center
        lat = 51.0504 + random.uniform(-0.06, 0.06)
        lon = 13.7373 + random.uniform(-0.08, 0.08)
        beds = random.randint(5, 80) if ftype != "Hospital" else random.randint(150, 500)
        docs = random.randint(4, 25) if ftype != "Hospital" else random.randint(40, 110)
        resp = random.uniform(8.0, 18.0)
        healthcare_facilities.append((dist_id, name, ftype, lat, lon, beds, docs, resp))
        
    cursor.executemany(
        """INSERT INTO healthcare_resources 
           (district_id, name, type, latitude, longitude, beds, doctors, emergency_response_time_min) 
           VALUES (?, ?, ?, ?, ?, ?, ?, ?)""",
        healthcare_facilities
    )
    conn.commit()

    # 4. Generate Demographics & Epidemiology (50,000+ records)
    print("Generating 50,000+ demographics & epidemiology rows...")
    
    demographics_rows = []
    epidemiology_rows = []
    
    # We will generate individual resident records
    num_records = 52000  # Slightly above 50,000 to be safe and satisfy "at least 50k"
    
    years = [2020, 2021, 2022, 2023, 2024, 2025]
    
    # Helper lists for choice weights
    district_weights = [0.12, 0.15, 0.11, 0.07, 0.06, 0.09, 0.10, 0.11, 0.11, 0.08]
    education_levels = ["Primary", "Secondary", "Tertiary"]
    
    # Open CSV files to write raw records
    demographics_csv_path = os.path.join(RAW_DATA_DIR, "demographics.csv")
    epidemiology_csv_path = os.path.join(RAW_DATA_DIR, "epidemiology.csv")
    
    demographics_headers = [
        "id", "district_name", "year", "age", "gender", "income", 
        "education", "employed", "life_expectancy"
    ]
    epidemiology_headers = [
        "demographic_id", "diabetes", "heart_disease", "obesity", 
        "smoking", "vaccinated", "mental_health_index"
    ]
    
    dem_file = open(demographics_csv_path, mode='w', newline='', encoding='utf-8')
    epi_file = open(epidemiology_csv_path, mode='w', newline='', encoding='utf-8')
    
    dem_writer = csv.writer(dem_file)
    epi_writer = csv.writer(epi_file)
    
    dem_writer.writerow(demographics_headers)
    epi_writer.writerow(epidemiology_headers)

    print("Populating individual tables...")
    for resident_id in range(1, num_records + 1):
        # Select District based on population weights
        dist_idx = random.choices(range(len(districts_data)), weights=district_weights)[0]
        dist_meta = districts_data[dist_idx]
        dist_name = dist_meta[0]
        dist_id = district_map[dist_name]
        
        # Select Year
        year = random.choice(years)
        
        # Age distribution dependent on district
        # Loschwitz/Blasewitz are older, Neustadt is younger
        if dist_name in ["Loschwitz", "Blasewitz"]:
            age = int(random.normalvariate(55, 18))
        elif dist_name == "Neustadt":
            age = int(random.normalvariate(29, 12))
        else:
            age = int(random.normalvariate(41, 20))
            
        # Bound age between 0 and 98
        age = max(0, min(98, age))
        
        gender = random.choices(["Male", "Female"], weights=[0.49, 0.51])[0]
        
        # Socio-economic generation based on district baselines
        base_income = dist_meta[3]
        # Perturb income
        income = int(random.normalvariate(base_income, base_income * 0.25))
        income = max(8000, income) # Floor income
        
        # Education distribution
        if base_income > 50000:
            edu_weights = [0.10, 0.35, 0.55] # highly educated
        elif base_income < 30000:
            edu_weights = [0.35, 0.50, 0.15] # lower education
        else:
            edu_weights = [0.20, 0.55, 0.25]
        education = random.choices(education_levels, weights=edu_weights)[0]
        
        # Employment rate (depends on education and district)
        emp_prob = 0.85
        if education == "Primary":
            emp_prob -= 0.15
        elif education == "Tertiary":
            emp_prob += 0.10
        if dist_name == "Prohlis":
            emp_prob -= 0.10
        elif dist_name == "Loschwitz":
            emp_prob += 0.05
            
        emp_prob = max(0.5, min(0.98, emp_prob))
        employed = 1 if (random.random() < emp_prob and age >= 18 and age <= 67) else 0
        if age < 18 or age > 67:
            employed = 0 # students/retirees
            
        # Smoking probability (lifestyle choice)
        smoke_prob = 0.24 # base national average
        if education == "Primary":
            smoke_prob += 0.12
        elif education == "Tertiary":
            smoke_prob -= 0.10
        if dist_name in ["Cotta", "Prohlis", "Neustadt"]:
            smoke_prob += 0.06
        smoke_prob = max(0.05, min(0.50, smoke_prob))
        smoking = 1 if random.random() < smoke_prob else 0
        
        # Life expectancy calculation based on resident risk factors
        base_life_exp = dist_meta[4]
        life_exp = base_life_exp
        if gender == "Female":
            life_exp += 4.5  # Women live longer
        if smoking:
            life_exp -= 8.0  # Smoking reduces life expectancy
        if education == "Primary":
            life_exp -= 3.0
        elif education == "Tertiary":
            life_exp += 2.0
        # Age adjustment
        if age > 75:
            life_exp = max(life_exp, age + random.uniform(1.0, 5.0))
            
        life_exp = round(life_exp, 1)
        
        # Append Demographics Record
        demographics_rows.append((resident_id, dist_id, year, age, gender, income, education, employed, life_exp))
        dem_writer.writerow([resident_id, dist_name, year, age, gender, income, education, employed, life_exp])
        
        # --- Epidemiology Model ---
        # Obesity probability (depends on age, income, district)
        obese_prob = 0.16 # base
        if age > 45:
            obese_prob += 0.08
        if income < 25000:
            obese_prob += 0.12
        if dist_name == "Prohlis":
            obese_prob += 0.08
        obese_prob = max(0.04, min(0.40, obese_prob))
        obesity = 1 if random.random() < obese_prob else 0
        
        # Diabetes probability (strongly linked to obesity and age)
        diab_prob = 0.08 # base
        if age > 50:
            diab_prob += 0.12
        if obesity:
            diab_prob += 0.20
        diab_prob = max(0.01, min(0.45, diab_prob))
        diabetes = 1 if random.random() < diab_prob else 0
        
        # Heart disease probability (linked to smoking, age, diabetes, gender)
        heart_prob = 0.06
        if age > 60:
            heart_prob += 0.18
        if smoking:
            heart_prob += 0.10
        if diabetes:
            heart_prob += 0.08
        if gender == "Male":
            heart_prob += 0.04
        heart_prob = max(0.01, min(0.50, heart_prob))
        heart_disease = 1 if random.random() < heart_prob else 0
        
        # Vaccination probability (lifestyle choice, depends on education and income)
        vac_prob = 0.72
        if education == "Tertiary":
            vac_prob += 0.12
        elif education == "Primary":
            vac_prob -= 0.10
        if dist_name == "Prohlis":
            vac_prob -= 0.08
        vac_prob = max(0.40, min(0.95, vac_prob))
        vaccinated = 1 if random.random() < vac_prob else 0
        
        # Mental health index (0 to 100, where 100 is best)
        # Depends on income, employment, age, noise levels
        mental_health = 75.0
        if employed == 0 and age >= 18 and age <= 65:
            mental_health -= 15.0 # unemployment stress
        if income < 20000:
            mental_health -= 8.0
        noise = dist_meta[6]
        if noise > 65:
            mental_health -= 5.0 # noise pollution impact
        # age effect (U-shape)
        if age >= 18 and age <= 35:
            mental_health -= 4.0
        elif age > 70:
            mental_health -= 6.0 # isolation
            
        mental_health += random.normalvariate(0, 8)
        mental_health = round(max(10.0, min(100.0, mental_health)), 1)
        
        # Append Epidemiology Record
        epidemiology_rows.append((resident_id, diabetes, heart_disease, obesity, smoking, vaccinated, mental_health))
        epi_writer.writerow([resident_id, diabetes, heart_disease, obesity, smoking, vaccinated, mental_health])
        
        # Batch commit to database every 10k records to keep memory footprint low
        if resident_id % 10000 == 0:
            print(f"Generated {resident_id} residents...")
            
    dem_file.close()
    epi_file.close()
    
    # Bulk insert into SQLite
    print("Writing demographics to database...")
    cursor.executemany(
        """INSERT INTO demographics 
           (id, district_id, year, age, gender, income, education, employed, life_expectancy) 
           VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)""",
        demographics_rows
    )
    conn.commit()
    
    print("Writing epidemiology to database...")
    cursor.executemany(
        """INSERT INTO epidemiology 
           (demographic_id, diabetes, heart_disease, obesity, smoking, vaccinated, mental_health_index) 
           VALUES (?, ?, ?, ?, ?, ?, ?)""",
        epidemiology_rows
    )
    conn.commit()

    # 5. Generate Environmental logs (21,900 rows)
    # Daily logs for each district across 6 years (2020-01-01 to 2025-12-31)
    print("Generating daily environmental logs (21,900 rows)...")
    env_rows = []
    
    start_date = datetime(2020, 1, 1)
    end_date = datetime(2025, 12, 31)
    delta = end_date - start_date
    num_days = delta.days + 1
    
    env_csv_path = os.path.join(RAW_DATA_DIR, "environmental.csv")
    env_file = open(env_csv_path, mode='w', newline='', encoding='utf-8')
    env_writer = csv.writer(env_file)
    env_writer.writerow(["district_name", "date", "pm25", "temperature", "green_space_pct", "noise_level_db", "water_quality_index"])
    
    # Cache date strings and seasonal factors
    date_factors = []
    for d in range(num_days):
        current_date = start_date + timedelta(days=d)
        date_str = current_date.strftime("%Y-%m-%d")
        day_of_year = current_date.timetuple().tm_yday
        # Sinusoidal temperature wave: peak in July (day ~200), minimum in Jan (day ~15)
        # Math: cos(2*pi*(day - 200)/365)
        seasonal_temp = 12.0 + 10.0 * math.cos(2 * math.pi * (day_of_year - 200) / 365)
        # PM2.5 is higher in winter due to heating and inversions
        seasonal_pm25 = 1.3 - 0.4 * math.cos(2 * math.pi * (day_of_year - 200) / 365)
        date_factors.append((date_str, seasonal_temp, seasonal_pm25))
        
    for name, district_id in district_map.items():
        # Get district base settings
        dist_meta = [d for d in districts_data if d[0] == name][0]
        base_green = dist_meta[5]
        base_noise = dist_meta[6]
        
        # Select base PM2.5 for this district (denser districts have higher pollution)
        base_pm25 = 12.0
        if name in ["Altstadt", "Neustadt"]:
            base_pm25 = 18.0
        elif name in ["Cotta", "Prohlis"]:
            base_pm25 = 15.0
        elif name in ["Loschwitz", "Klotzsche"]:
            base_pm25 = 7.5
            
        for d in range(num_days):
            date_str, seasonal_temp, seasonal_pm25factor = date_factors[d]
            
            # Apply randomness to environmental parameters
            pm25 = base_pm25 * seasonal_pm25factor + random.normalvariate(0, 2.5)
            pm25 = max(1.5, round(pm25, 2))
            
            temp = seasonal_temp + random.normalvariate(0, 3.0)
            temp = round(temp, 1)
            
            # Green space slightly fluctuates or stays constant
            green = base_green + random.uniform(-0.5, 0.5)
            green = max(2.0, min(95.0, round(green, 2)))
            
            # Noise level fluctuates slightly
            noise = base_noise + random.normalvariate(0, 1.5)
            noise = max(30.0, min(90.0, round(noise, 1)))
            
            # Water quality index (0-100)
            water = 82.0 + random.normalvariate(0, 4.0)
            if name == "Prohlis":
                water -= 6.0
            elif name == "Loschwitz":
                water += 5.0
            water = max(40.0, min(100.0, round(water, 1)))
            
            env_rows.append((district_id, date_str, pm25, temp, green, noise, water))
            env_writer.writerow([name, date_str, pm25, temp, green, noise, water])
            
    env_file.close()
    
    print("Writing environmental logs to database...")
    cursor.executemany(
        """INSERT INTO environmental 
           (district_id, date, pm25, temperature, green_space_pct, noise_level_db, water_quality_index) 
           VALUES (?, ?, ?, ?, ?, ?, ?)""",
        env_rows
    )
    conn.commit()
    conn.close()
    print("Database seeding completed successfully!")

if __name__ == "__main__":
    create_database()
    generate_and_seed_data()
