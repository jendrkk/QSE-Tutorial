import time
import pandas as pd
import numpy as np
from MODEL import MODEL
from MODEL_JIT import MODEL_JIT

def benchmark():
    # Expanded city sizes (n + 1 for unique center)
    city_sizes = [1001, 2001, 5001, 10001, 20001, 50001, 100001, 200001]
    
    results = []

    # Common guesses
    y_guess = 2.5
    L_guess = 1_000_000.0

    print("Warming up JIT compiler...")
    # Warm up with a small size once to trigger compilation of all jitted functions
    warmup_model = MODEL_JIT(city_size=101)
    warmup_model.find_eq_simple_jit(y_guess, L_guess)
    warmup_model.find_eq_bisection_jit()
    warmup_model.find_eq_advanced_revised(y_guess, L_guess)
    print("Warm-up complete.\n")

    print(f"{'City Size':<12} | {'Algorithm':<15} | {'Model Type':<12} | {'Time (s)':<10}")
    print("-" * 60)

    for size in city_sizes:
        # Initialize models
        m_py = MODEL(city_size=size)
        m_jit = MODEL_JIT(city_size=size)

        algorithms = [
            ("Simple", m_py.find_eq_simple, m_jit.find_eq_simple_jit, (y_guess, L_guess)),
            ("Bisection", m_py.find_eq_bisection, m_jit.find_eq_bisection_jit, ()),
            ("Adv Revised", m_py.find_eq_advanced_revised, m_jit.find_eq_advanced_revised, (y_guess, L_guess))
        ]

        for alg_name, py_func, jit_func, args in algorithms:
            # Benchmark Python
            start = time.perf_counter()
            py_func(*args)
            py_time = time.perf_counter() - start
            results.append({
                'City Size': size,
                'Algorithm': alg_name,
                'Model': 'Python',
                'Time': py_time
            })
            print(f"{size:<12} | {alg_name:<15} | {'Python':<12} | {py_time:.4f}")

            # Benchmark JIT
            start = time.perf_counter()
            jit_func(*args)
            jit_time = time.perf_counter() - start
            results.append({
                'City Size': size,
                'Algorithm': alg_name,
                'Model': 'JIT',
                'Time': jit_time
            })
            print(f"{size:<12} | {alg_name:<15} | {'JIT':<12} | {jit_time:.4f}")

    # Process and summarize
    df = pd.DataFrame(results)
    
    print("\n" + "="*60)
    print("SUMMARY: JIT SPEEDUP BY ALGORITHM AND CITY SIZE")
    print("="*60)
    
    summary = df.pivot(index=['City Size', 'Algorithm'], columns='Model', values='Time')
    summary['Speedup'] = summary['Python'] / summary['JIT']
    print(summary)

    print("\n" + "="*60)
    print("AVERAGE SPEEDUP ACROSS ALL SIZES")
    print("="*60)
    avg_speedup = summary.groupby('Algorithm')['Speedup'].mean()
    print(avg_speedup)

    print("\n" + "="*60)
    print("AVERAGE TIME BY ALGORITHM (JIT ONLY)")
    print("="*60)
    avg_time_jit = df[df['Model'] == 'JIT'].groupby('Algorithm')['Time'].mean()
    print(avg_time_jit.sort_values())

if __name__ == "__main__":
    benchmark()
