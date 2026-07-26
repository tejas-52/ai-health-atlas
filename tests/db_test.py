# Database and Query Verification Test Suite (Python)
import os
import sqlite3
import hashlib

DB_PATH = os.path.join("database", "health_atlas.db")

def run_tests():
    print("========================================")
    print("STARTING DATABASE & BACKEND QUERY TESTS")
    print("========================================")
    
    if not os.path.exists(DB_PATH):
        print(f"[ERROR] Database file not found at {DB_PATH}")
        return False
        
    conn = sqlite3.connect(DB_PATH)
    cursor = conn.cursor()
    
    # Test 1: Check Table Structures and Seeding Counts
    print("\n[Test 1] Verifying table sizes and data integrity...")
    tables = {
        "districts": 10,
        "demographics": 52000,
        "epidemiology": 52000,
        "environmental": 21900,
        "healthcare_resources": 22,
        "users": 3
    }
    
    for table, expected_min in tables.items():
        cursor.execute(f"SELECT COUNT(*) FROM {table}")
        count = cursor.fetchone()[0]
        if count >= expected_min:
            print(f"  [OK] Table '{table}': PASSED ({count} rows, expected >= {expected_min})")
        else:
            print(f"  [FAIL] Table '{table}': FAILED ({count} rows, expected >= {expected_min})")
            
    # Test 2: Verify Credentials and Hashing
    print("\n[Test 2] Verifying pre-seeded user authentication...")
    test_users = [
        ("admin", "admin123", "Admin"),
        ("researcher", "research123", "Researcher"),
        ("viewer", "view123", "Viewer")
    ]
    
    for username, password, expected_role in test_users:
        pass_hash = hashlib.sha256(password.encode('utf-8')).hexdigest()
        cursor.execute("SELECT role FROM users WHERE username = ? AND password_hash = ?", (username, pass_hash))
        res = cursor.fetchone()
        if res and res[0] == expected_role:
            print(f"  [OK] Credentials for '{username}' ({expected_role}): PASSED")
        else:
            print(f"  [FAIL] Credentials for '{username}': FAILED")
            
    # Test 3: Validate Dashboard Cohort Aggregation SQL
    print("\n[Test 3] Validating dashboard group-by cohort aggregation SQL...")
    try:
        # Simulating dashboard_data() aggregation query
        query = """
            SELECT 
              d.name AS district_name,
              dem.year,
              dem.gender,
              CASE 
                WHEN dem.age BETWEEN 0 AND 17 THEN '0-17'
                WHEN dem.age BETWEEN 18 AND 34 THEN '18-34'
                WHEN dem.age BETWEEN 35 AND 49 THEN '35-49'
                WHEN dem.age BETWEEN 50 AND 64 THEN '50-64'
                ELSE '65+'
              END AS age_group,
              COUNT(dem.id) AS cohort_size,
              AVG(dem.income) AS avg_income,
              AVG(dem.life_expectancy) AS avg_life_expectancy,
              SUM(dem.employed) * 100.0 / COUNT(dem.id) AS employment_rate,
              SUM(epi.diabetes) * 100.0 / COUNT(dem.id) AS diabetes_rate,
              SUM(epi.heart_disease) * 100.0 / COUNT(dem.id) AS heart_disease_rate,
              SUM(epi.obesity) * 100.0 / COUNT(dem.id) AS obesity_rate,
              SUM(epi.smoking) * 100.0 / COUNT(dem.id) AS smoking_rate,
              SUM(epi.vaccinated) * 100.0 / COUNT(dem.id) AS vaccination_rate,
              AVG(epi.mental_health_index) AS avg_mental_health_index
            FROM demographics dem
            JOIN districts d ON dem.district_id = d.id
            JOIN epidemiology epi ON dem.id = epi.demographic_id
            WHERE d.name IN ('Altstadt', 'Neustadt') AND dem.year = 2025
            GROUP BY district_name, year, gender, age_group
        """
        cursor.execute(query)
        rows = cursor.fetchall()
        print(f"  [OK] SQL Aggregation Query: PASSED (Returned {len(rows)} cohorts)")
    except Exception as e:
        print(f"  [FAIL] SQL Aggregation Query: FAILED ({e})")
        
    # Test 4: Validate Chatbot NLP SQL queries
    print("\n[Test 4] Validating Chatbot query resolutions...")
    try:
        # Query 1: Highest Obesity
        cursor.execute("""
            SELECT d.name, AVG(epi.obesity) * 100.0 AS rate 
            FROM epidemiology epi
            JOIN demographics dem ON epi.demographic_id = dem.id
            JOIN districts d ON dem.district_id = d.id
            GROUP BY d.name ORDER BY rate DESC LIMIT 1
        """)
        res_ob = cursor.fetchone()
        print(f"  [OK] Query 'highest obesity': PASSED (Result: {res_ob[0]} at {res_ob[1]:.1f}%)")
        
        # Query 2: Healthcare accessibility comparison
        cursor.execute("""
            SELECT d.name, COUNT(h.id), SUM(h.beds), AVG(h.emergency_response_time_min)
            FROM districts d
            LEFT JOIN healthcare_resources h ON d.id = h.district_id
            GROUP BY d.name
        """)
        res_hc = cursor.fetchall()
        print(f"  [OK] Query 'compare healthcare': PASSED (Returned {len(res_hc)} districts)")
        
    except Exception as e:
        print(f"  [FAIL] Chatbot SQL Queries: FAILED ({e})")
        
    conn.close()
    print("\n========================================")
    print("TEST SUITE COMPLETED RUNNING")
    print("========================================")
    return True

if __name__ == "__main__":
    run_tests()
