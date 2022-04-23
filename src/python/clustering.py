import numpy as np
import pandas as pd
from scipy.sparse import csgraph
from scipy.sparse.linalg import eigsh
from scipy.spatial.distance import cdist
from sklearn import cluster
from clusim.clustering import Clustering, print_clustering
import clusim.sim as sim

def ari(x1, x2, random_model_type='perm1'):

    x1_clustering = Clustering()
    x1_clustering.from_membership_list(x1)
    x2_clustering = Clustering()
    x2_clustering.from_membership_list(x2)

    assert random_model_type in ('all1', 'num1', 'perm1', 'all', 'perm', 'num'), "Bad Random Type"

    return sim.adjrand_index(
        x1_clustering,
        x2_clustering,
        random_model=random_model_type
    )

def get_sim_matrix(X, dist_metric='cosine'):

    # dist_metric options are any function that takes two lists, returns scalar
    if "qualtrics_id" in X.columns:
        X = X.set_index('qualtrics_id')

    distance_matrix = cdist(X.values, X.values, metric=dist_metric)

    # apply some transformation tricks from comp vision 
    # produces smooth distribution over distances, than inverts ->
    # similarity matrix
    sigma = distance_matrix.std()
    #delta = 3 * sigma # rule of thumb
    delta = 3 * sigma # rule of thumb
    sim_matrix = np.exp(- distance_matrix ** 2 / (2. * delta ** 2))

    return pd.DataFrame(sim_matrix, index=X.index)

def eigenDecomposition(A):
    if isinstance(A, pd.DataFrame):
        A = A.values
    L = csgraph.laplacian(A, normed=True)
    evalues, evectors = eigsh(L, k=20, which="LM", sigma=1.0,
                              maxiter=5000)
    index_largest_gap = np.argmax(np.diff(evalues)) + 1
    nb_clusters = index_largest_gap + 1
    
    # Optional: add hard-coded check to see if nb_clusters is too small (< 2)

    return nb_clusters, evalues, evectors


def eigen_vectors(sim_matrix):
    _, evalues, _ = eigenDecomposition(sim_matrix)
    return evalues

def get_spectral_n_opt(sim_matrix):
    n_best, _, _ = eigenDecomposition(sim_matrix)
    return n_best

def get_spectral_clusters(sim_matrix, name, n_opt=None):
    if "qualtrics_id" in sim_matrix.columns:
        sim_matrix = sim_matrix.set_index("qualtrics_id")
    if n_opt is None:
        n_opt = get_spectral_n_opt(sim_matrix=sim_matrix.values)
    spectral_model = cluster.SpectralClustering(affinity='precomputed',
                                                        n_clusters=int(n_opt)).fit(sim_matrix.values)
    return pd.DataFrame({'cluster.id.{}'.format(name): spectral_model.labels_, 
                         'qualtrics_id': sim_matrix.index})

def get_kmeans_n_opt(X, k_range=range(2,20)):
    def opt_num_clusters(inertia):
        x1, y1 = min(k_range), inertia[0]
        x2, y2 = max(k_range), inertia[len(inertia) - 1]

        distances = list()
        for i in range(len(inertia)):
            x0 = i + min(k_range)
            y0 = inertia[i]
            numerator = abs(((y2 - y1) * x0) - ((x2 - x1) * y0) + (x2 * y1) - (y2 * x1))
            denominator = math.sqrt((y2 - y1)**2 + (x2 - x1)**2)
            distances.append(numerator / denominator)
        return distances.index(max(distances)) + min(k_range)

    # within-cluster sum of squares
    wcss = list()
    for j in k_range:
        km = cluster.KMeans(n_clusters=j, random_state=666).fit(X)
        wcss.append(km.inertia_)
    n_opt = opt_num_clusters(wcss)
    return n_opt

def get_kmeans_clusters(X, name, n_opt=None):
    if "qualtrics_id" in X.columns:
        X = X.set_index("qualtrics_id")
    if n_opt is None:
        n_opt = get_kmeans_n_opt(X)
    kmeans_opt = cluster.KMeans(n_clusters=n_opt).fit(X)
    return pd.DataFrame({'cluster.id.{}'.format(name): kmeans_opt.labels_, 
                         'qualtrics_id': X.index})
