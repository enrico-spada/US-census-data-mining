import pandas as pd
import numpy as np
from sklearn import preprocessing
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from pickle import dump
from pickle import load



dump(target_encode, open("../models/preprocessing/target_encode.pkl", "wb"))

cat_impute

scaler = load(open('../models/preprocessing/target_encode.pkl', 'rb'))
list(scaler.classes_)

scaler.transform(['- 50000.', '50000+.'])


for name in dir():
    if not name.startswith('_'):
        del globals()[name]

filepath =  "../data/1_input_data/input_data_94.csv"

res = preparation(filepath)

def preparation(filepath):
        #Read metadata for Python data types
        df_dtypes = pd.read_csv("../docs/metadata/census_income_metadata-prepared.v00.csv", index_col = "description").p_types.to_dict()
        print("Read metadata")
        
        #Read data source
        df_input  =pd.read_csv(filepath, dtype = df_dtypes)
        print("Read data source")
        
        #Calculate lists for feature names
        # categorical_features = df_input.select_dtypes(include=['object']).columns.values
        categorical_features = [key for key, value in df_dtypes.items() if value == "object" and key != "target"]
        measurable_features = [key for key, value in df_dtypes.items() if value != "object"]
        all_features = [key for key in df_dtypes.keys()]
        
        #Remove duplicates
        df_input_dedup = df_input.drop_duplicates()
        print("Dropped " + str(df_input.shape[0] - df_input_dedup.shape[0]) + " duplicates" )
        
        #Categorical missing
        #df_input_dedup_impute_cat = df_input_dedup.copy()
        #[df_input_dedup_impute_cat[col].fillna("Missing", inplace = True) for col in df_input_dedup_impute_cat.columns if df_input_dedup[col].isna().any() and col in categorical_features]
        
        cols = categorical_features + [feature for feature in all_features if feature not in categorical_features]
        
        prep_pipeline = ColumnTransformer(transformers=[('cat_missing', SimpleImputer(strategy="constant", fill_value = "Missing"), categorical_features),
        ("std_num", preprocessing.StandardScaler(), measurable_features)],remainder = "passthrough")
        np_input_dedup_impute_cat = prep_pipeline.fit_transform(df_input_dedup)
        df_input_dedup_impute_cat = pd.DataFrame(data=np_input_dedup_impute_cat, columns=cols)
                
        df_input_dedup_impute_cat_remap_target = df_input_dedup_impute_cat
        
        #Train transformers
        target_encode = preprocessing.LabelEncoder()
        target_encode.fit(["- 50000.", "50000+."])
        list(target_encode.classes_)
        
        #Remap target
        df_input_dedup_impute_cat_remap_target.loc[: , "target"] = target_encode.transform(df_input_dedup_impute_cat_remap_target["target"])
        
        #rearrange column order
        df_prep_output = df_input_dedup_impute_cat_remap_target[all_features]
        
        return df_prep_output
        
res.to_csv("../data/2_eda_prep/df_prepared.csv", index = False)

#Stratified sampling
from sklearn import model_selection
X = res.drop("target", axis = 1)
X = res.drop("target", axis = 1).to_numpy()
y = res["target"].array

cv_split = model_selection.StratifiedShuffleSplit(n_splits = 10, test_size = .3, train_size = .6, random_state = 0) # run model 10x with 60/30 split intentionally leaving out 10%
cv_split.get_n_splits(X, y)
print(cv_split)

i = 0
for train_index, test_index in cv_split.split(X, y):
    i += 1
    pd.DataFrame(data = train_index, columns = ["index"]).to_csv("../models/sampling/cross_val_stratified_train_"+str(i)+".csv", index = False)
    pd.DataFrame(data = train_index, columns = ["index"]).to_csv("../models/sampling/cross_val_stratified_test_"+str(i)+".csv", index = False)



from sklearn.ensemble import RandomForestClassifier
clf = RandomForestClassifier(max_depth=5, random_state=0)
clf.fit(X, y)

print(clf.feature_importances_)

print(clf.predict([[0, 0, 0, 0]]))








# example of fitting a model on the scaled dataset
from sklearn.datasets import make_blobs
from sklearn.model_selection import train_test_split
from sklearn.preprocessing import MinMaxScaler
from sklearn.linear_model import LogisticRegression
from pickle import dump
# prepare dataset
X, y = make_blobs(n_samples=100, centers=2, n_features=2, random_state=1)
# split data into train and test sets
X_train, _, y_train, _ = train_test_split(X, y, test_size=0.33, random_state=1)
# define scaler
scaler = MinMaxScaler()
# fit scaler on the training dataset
scaler.fit(X_train)
# transform the training dataset
X_train_scaled = scaler.transform(X_train)
# define model
model = LogisticRegression(solver='lbfgs')
model.fit(X_train_scaled, y_train)
# save the model
dump(model, open('model.pkl', 'wb'))
# save the scaler
dump(scaler, open('scaler.pkl', 'wb'))


# load model and scaler and make predictions on new data
from sklearn.datasets import make_blobs
from sklearn.model_selection import train_test_split
from sklearn.metrics import accuracy_score
from pickle import load
# prepare dataset
X, y = make_blobs(n_samples=100, centers=2, n_features=2, random_state=1)
# split data into train and test sets
_, X_test, _, y_test = train_test_split(X, y, test_size=0.33, random_state=1)
# load the model
model = load(open('model.pkl', 'rb'))
# load the scaler
scaler = load(open('scaler.pkl', 'rb'))
# transform the test dataset
X_test_scaled = scaler.transform(X_test)
# make predictions on the test set
yhat = model.predict(X_test_scaled)
# evaluate accuracy
acc = accuracy_score(y_test, yhat)
print('Test Accuracy:', acc)
    
