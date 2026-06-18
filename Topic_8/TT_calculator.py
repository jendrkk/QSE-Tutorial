import networkx as nx
import igraph as ig
import pandas as pd
import numpy as np
import time
from joblib import Parallel, delayed

def build_networkx_graph(edges_df):
    """Builds a NetworkX graph for path tracing."""
    G = nx.Graph()
    for _, row in edges_df.iterrows():
        G.add_edge(row['node_id_from'], row['node_id_to'], weight=row['travel_time_min'], edge_type=row['edge_type'])
    return G

def trace_dijkstra_path(G, source, target):
    """Traces and prints a route breakdown between two nodes."""
    if not nx.has_path(G, source, target):
        print(f"No path found between {source} and {target}")
        return
    
    path = nx.shortest_path(G, source=source, target=target, weight='weight')
    total_time = nx.shortest_path_length(G, source=source, target=target, weight='weight')
    
    print(f"Fastest Route from {source} to {target}: {total_time:.2f} mins")
    for i in range(len(path) - 1):
        u, v = path[i], path[i+1]
        data = G[u][v]
        print(f"  Step {i+1:02d}: {u} -> {v} | Type: {data['edge_type']:<22} | Cost: {data['weight']:5.2f} mins")

def compute_travel_time_matrix(edges_df, centroids, n_jobs=-1, batch_size=500):
    """Solves N x N travel times using parallelized IGraph."""
    all_nodes = pd.concat([edges_df['node_id_from'], edges_df['node_id_to']]).unique()
    node_to_idx = {node: idx for idx, node in enumerate(all_nodes)}
    
    g = ig.Graph(directed=False)
    g.add_vertices(len(all_nodes))
    g.vs["name"] = all_nodes
    
    edge_list = list(zip(edges_df['node_id_from'].map(node_to_idx), edges_df['node_id_to'].map(node_to_idx)))
    g.add_edges(edge_list)
    g.es["weight"] = edges_df['travel_time_min'].values
    
    valid_centroids = [c for c in centroids if c in node_to_idx]
    centroid_indices = np.array([node_to_idx[c] for c in valid_centroids])
    num_valid = len(valid_centroids)
    
    result_matrix = np.full((len(centroids), len(centroids)), np.nan, dtype=np.float32)
    orig_pos_map = {c: i for i, c in enumerate(centroids)}
    valid_positions = np.array([orig_pos_map[c] for c in valid_centroids])
    
    start_time = time.time()
    def solve_batch(i):
        batch_sources = centroid_indices[i : min(i + batch_size, num_valid)]
        return g.distances(source=batch_sources, target=centroid_indices, weights="weight")

    results = Parallel(n_jobs=n_jobs, batch_size=1)(
        delayed(solve_batch)(i) for i in range(0, num_valid, batch_size)
    )
    
    curr_idx = 0
    for batch_results in results:
        num_in_batch = len(batch_results)
        batch_orig_positions = valid_positions[curr_idx : curr_idx + num_in_batch]
        for b_idx, orig_row_idx in enumerate(batch_orig_positions):
            result_matrix[orig_row_idx, valid_positions] = batch_results[b_idx]
        curr_idx += num_in_batch
        
    print(f"Computed matrix in {(time.time() - start_time)/60:.2f} minutes.")
    return pd.DataFrame(result_matrix, index=centroids, columns=centroids)