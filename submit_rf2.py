from collections import defaultdict
import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
import util

def format_data(group, tr, ts, type):
    if type=='base':
        feats = ["MBase", "MSecond", "MSeries", "MDesc", "pre", "MfgID", "Year", "Age.imp", "YearMade.imp", "SizeL.med.imp", "SizeU.med.imp", "DriveSys", "Enc", "Forks", "Pad", "Ride", "Stick", "Trans", "Turbo", "Blade", "EncType", "HP", "Hydraulics", "Pushblock", "Ripper", "Scarifier", "Tip", "Tire.imp", "Coupler", "CouplerSys", "Grouser", "HydraFlow", "Track", "Thumb" "Pattern", "GrouserType", "Backhoe", "BladeType", "Travel", "Differ", "Steering", "cpi_us.imp", "cpi_trucks.imp", "pc_agri.imp", "pc_contr.imp", "wp_agri.imp", "wp_contr.imp"]
    else:
        feats = ["MBase", "MSecond", "MSeries", "MDesc", "pre", "MfgID", "Year", "Month", "Age.imp", "YearMade.imp", "SizeL.med.imp", "SizeU.med.imp", "DriveSys", "Enc", "Forks", "Pad", "Ride", "Stick", "Trans", "Turbo", "Blade", "EncType", "HP", "Hydraulics", "Pushblock", "Ripper", "Scarifier", "Tip", "Tire.imp", "Coupler", "CouplerSys", "Grouser", "HydraFlow", "Track", "Thumb", "Pattern", "GrouserType", "Backhoe", "BladeType", "Travel", "Differ", "Steering", "cpi_us.imp", "cpi_trucks.imp", "pc_agri.imp", "pc_contr.imp", "wp_agri.imp", "wp_contr.imp", "Age.med.imp", "YearMade.med.imp", "Age.min.imp", "YearMade.min.imp", "SizeL.imp", "SizeU.imp", "Source", "AucID" "State", "market", "models"]

    tr_feats = pd.DataFrame(tr["Month"], index=tr["Month"].index)
    ts_feats = pd.DataFrame(ts["Month"], index=ts["Month"].index)
            
    columns = set(tr.columns)
    # columns.remove("SalesID")
    columns.remove("SalePrice")
    columns.remove("Month")
    
    for col in columns.intersection(feats):
        if tr[col].dtype == np.dtype('object'):
            s = np.unique(tr[col].values)
            mapping = pd.Series([x[0] for x in enumerate(s)], index = s)
            tr_feats = tr_feats.join(tr[col].map(mapping))
            ts_feats = ts_feats.join(ts[col].map(mapping))
        else:
            tr_feats = tr_feats.join(tr[col])
            ts_feats = ts_feats.join(ts[col])

    return tr_feats, ts_feats, tr["SalePrice"], ts["SalePrice"]

if __name__ == '__main__':        
    data_types = ['-orig6', '-rank6']
    feat_types = ['base', 'many']
    groups = ['TTT', 'WL', 'TEX', 'BL', 'MG', 'SSL']
    max_features = {'-orig6': {'base': [7, 5, 6, 6, 10, 6], 'many': [8, 7, 5, 8, 11, 6]}, '-rank6': {'base': [8, 6, 5, 5, 12, 6], 'many': [8, 5, 5, 5, 10, 6]}}

    for data_type in data_types:

        for i, group in enumerate(groups):
            tr = util.get_data(fname='tr-' + group + data_type +  '.csv')
            ts = util.get_data(fname='ts-' + group + data_type +  '.csv')
        
            for feat_type in feat_types:
                print data_type, group, feat_type
                    
                train, test, y_tr, y_ts = format_data(group, tr, ts, feat_type)
                rf = RandomForestRegressor(n_estimators=800, n_jobs=4, min_samples_split=25, max_features=max_features[data_type][feat_type][i], compute_importances=True)
                rf.fit(train, y_tr)  
                p = rf.predict(test)
                imp = sorted(zip(train.columns, rf.feature_importances_), key=lambda tup: tup[1], reverse=True)
                for fea in imp:
                    print(fea)
                    
                util.write_submission("rf" + data_type + '-' + group + '-' + feat_type + ".csv", p.tolist())
                