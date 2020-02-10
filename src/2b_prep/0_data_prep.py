import pandas as pd
import numpy as np
from sklearn import preprocessing
from sklearn.compose import ColumnTransformer
from sklearn.impute import SimpleImputer
from pickle import dump
from pickle import load


def data_prep_to_csv(source_path, type, out_name):
	#INPUT
			#source_path: file path of the file to prepare or process. CSV required
			#type: one of "preparation" or "preprocessing"
	 
	#OUTPUT
			#output = dataframe
		
	#CHECK
			#Check type argument
	valid_type = {"preparation", "preprocessing"}
	if type not in valid_type:
		raise ValueError("Error: type must be one of %r." % valid_type)
		
		
	#READ DATA		
	#Read metadata for Python data types
	df_dtypes = pd.read_csv("../docs/metadata/census_income_metadata-prepared.v00.csv", index_col = "description").p_types.to_dict()
	print("Read metadata.")
	
	#Read data source
	df_input	=pd.read_csv(source_path, dtype = df_dtypes)
	print("Read data source.")
	
	#Calculate lists for feature names
	# categorical_features = df_input.select_dtypes(include=['object']).columns.values
	categorical_features = [key for key, value in df_dtypes.items() if value == "object" and key != "target"]
	measurable_features = [key for key, value in df_dtypes.items() if value != "object"]
	all_features = [key for key in df_dtypes.keys()]
	
	
	#PIPELINE
	if type == "preparation":
		#Define data prep pipeline with categorical missing imputation and numerical standardization
		prep_pipeline = ColumnTransformer(transformers=[('cat_missing', SimpleImputer(strategy="constant", fill_value = "Missing"), categorical_features),
		("std_num", preprocessing.StandardScaler(), measurable_features)],remainder = "passthrough")
		
		#Save pipeline for later re-use in post-processing
		dump(prep_pipeline, open('../models/preprocessing/prep_pipeline.pkl', 'wb'))
		
		#Define data prep label encoder for target
		target_encode = preprocessing.LabelEncoder()
		target_encode.fit(["- 50000.", "50000+."])
		
		#Save label encoder for later re-use in post-processing
		dump(target_encode, open('../models/preprocessing/target_encode.pkl', 'wb'))
		
		
	elif type == "preprocessing":
		try:
			#Load trained pipeline for categorical missing imputation and numerical standardization
			prep_pipeline = load(open('../models/preprocessing/prep_pipeline.pkl', 'rb'))
			
			#Load trained pipeline for target label encoding
			target_encode = load(open('../models/preprocessing/target_encode.pkl', 'rb'))
			
		except:
			print("No data prep pipeline defined on training data. Please perform preparation on training data first.")
			
			
	#Remove duplicates
	df_input_dedup = df_input.drop_duplicates()
	print("Dropped " + str(df_input.shape[0] - df_input_dedup.shape[0]) + " duplicates" )
	
	#Keep column order because scikit transformer rearrange columns
	cols = categorical_features + [feature for feature in all_features if feature not in categorical_features]
	
	#Apply data prep pipeline for impute categorical missing and scale numericl
	np_input_dedup_prepared = prep_pipeline.fit_transform(df_input_dedup)
	df_input_dedup_prepared = pd.DataFrame(data=np_input_dedup_prepared, columns=cols)
	df_input_dedup_prepared_target_encoded = df_input_dedup_prepared
	
	#Train transformers
	target_encode = preprocessing.LabelEncoder()
	target_encode.fit(["- 50000.", "50000+."])
	#list(target_encode.classes_)
	
	#Remap target
	df_input_dedup_prepared_target_encoded.loc[: , "target"] = target_encode.transform(df_input_dedup_prepared_target_encoded["target"])
	
	#Rearrange column order
	df_prep_output = df_input_dedup_prepared_target_encoded[all_features]
	
	#Save output as csv
	df_prep_output.to_csv("../data/2_eda_prep/"+out_name +".csv", index = False)
	return "Data " + type + " completed."
