import pandas as pd
import os
from sklearn.metrics import mean_squared_error as ms
from math import sqrt
import csv

def get_paths():
    """
    Redefine data_path and submissions_path here to run the benchmarks on your machine
    """
    data_path = os.path.join("c:\\", "FastIron", "Data")
    submission_path = os.path.join("c:\\", "FastIron", "Submission")
    return data_path, submission_path

def get_data(data_path = None, fname="Train.csv"):
    if data_path is None:
        data_path, submission_path = get_paths()
    df = pd.read_csv(os.path.join(data_path, fname))
    return df

# def write_submission(submission_name, predictions, submission_path=None):
    # if submission_path is None:
        # data_path, submission_path = get_paths()
    
    # test = get_test_df()    
    # test = test.join(pd.DataFrame({"SalePrice": predictions}))

    # test[["SalesID", "SalePrice"]].to_csv(os.path.join(submission_path,
        # submission_name), index=False)
  
# def write_blend(submission_name, predictions, submission_path=None):
    # if submission_path is None:
        # data_path, submission_path = get_paths()    
    # np.savetxt(os.path.join(submission_path, submission_name), predictions, delimiter=',')
    # f = open(os.path.join(submission_path, submission_name), 'ab')    
    # w = csv.writer(f)
    # w.writerow(predictions)
    # f.close()
    
def write_submission(submission_name, predictions, submission_path=None):
    if submission_path is None:
        data_path, submission_path = get_paths()    
    f = open(os.path.join(submission_path, submission_name), 'wb')    
    for p in predictions:
        f.write(str(p) + '\n')
    f.close()
    
def rms(o, p):
    return sqrt(ms(o,p))