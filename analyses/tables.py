import json

appendix_letters = {
	"1a": "B",
	"1b": "C",
	"2": "D",
	"3rep": "E",
	"4rep": "F"
}

def cleanup_studyname(n):
	if n == "3rep":
		return "3's replication portion"
	if n == "4rep":
		return "4's replication portion"
	else:
		return n


def cleanup_studyname_in_header(n):
	if n == "3rep":
		return "3 (replication portion)"
	if n == "4rep":
		return "4 (replication portion)"
	else:
		return n

for study_name in [
		#"1a", "1b", "2", "3rep", "4rep"
	]:

	models = {}
	header = None
	for i, line in enumerate(open("output/data" + study_name + ".csv").readlines()):
		cells = json.loads( ("["+line[:-1]+"]") )
		if i==0:
			header = cells
		else:
			data = dict(zip(header, cells))
			model = data["model"]
			if model not in models:
				models[model] = {}
			portion = data["portion"]
			if portion not in models[model]:
				models[model][portion] = {}
			row = data["row"]
			if row not in models[model][portion]:
				models[model][portion][row] = []
			element = (data["col"], data["value"])
			models[model][portion][row].append(element)

	# {"Model 1: Colinear Predictors": {
	# 	"fixed": {
	# 		{
	# 			"(Intercept)": [
	# 				{"col": "Estimate", "value": "0"},
	# 				...
	# 			]
	# 		}
	# 	}
	# }}

	def get_number(row, column_name):
		if column_name not in row:
			return ""
		else:
			if column_name == "p" or column_name == "Pr(>|t|)":
				if round(row[column_name], 3) == 0:
					return "$p<0.001$"
				return "{:.3f}".format(row[column_name])
			if column_name in ["chisq", "t-value", "t", "t value"]:
				return "{:.2f}".format(row[column_name])
			else:
				return "{:.3f}".format(row[column_name])

	def cleanup_colname(colname):
		return {
			"chisq": """$\chi^2$""",
			"p": "$p$",
			"sd": "sd",
			"df": "$df$",
			"t value": "$t$",
			"t-value": "$t$",
			"Estimate": "Estimate",
			"Pr(>|t|)": "$p$",
			"Std. Error": "se",
			"cor": "cor"
		}[colname]

# surprisal:adjectiveexpensive	Estimate	
# surprisal:adjectiveold	Estimate	
# surprisal:adjectivetall

	def cleanup_rowname(rowname):
		if rowname == "surprisal_resid":
			return "Surprisal\\textsubscript{resid}"
		elif rowname == "syll_resid":
			return "Length\\textsubscript{resid}"
		elif rowname == "syllables":
			return "Length"
		elif rowname == "surprisal":
			return "Surprisal"
		words = rowname.split()
		if len(words) == 2 and words[1] == "Resid":
			return words[0] + "\\textsubscript{resid}"
		return "$\\mid$".join(rowname.split("|"))

	def sort_columns(column_names):
		if "t value" in column_names:
			column_names = ["Estimate", "Std. Error", "t value", "Pr(>|t|)"]
		elif "t-value" in column_names:
			column_names = ["Estimate", "Std. Error", "t-value", "Pr(>|t|)"]
		elif "cor" in column_names:
			column_names = ["sd", "cor"]
		elif "chisq" in column_names:
			column_names = ["chisq", "p"]
		return column_names

	def sort_rows(row_names):
		row_names.sort(key=lambda s: s[0])
		return row_names

	table_content = """
\subsection{Appendix """ + appendix_letters[study_name] + """}

Results of linear mixed-effects models of scaled responses in Study """ + cleanup_studyname(study_name) + """ as a function of length in syllables and surprisal.
Model 1 includes raw predictors.
In Model 2, we replace the length predictor by the residuals when length is modeled as a linear function of surprisal.
In Model 3, we replace the surprisal predictor by the residuals when surprisal is modeled as a linear function of length.
Finally, we present results of likelihood ratio tests when each of the predictors is removed from the full model.

\\vspace{4mm}

\\noindent
\\footnotesize{
\\begin{tabular}{r|cccc}
\\hline
\\hline
\\multicolumn{5}{c}{\\textbf{Study """ + cleanup_studyname_in_header(study_name) + """ Regression Results}} \\\\
"""


	# # table_content = "\\begin{table}\n\\caption{Study " + study_name + " Regression Results}\n"
	# table_content = "\\begin{tabular}{r|cccc}\n"

	def make_model_name_line(model_name_line):
		return "\multicolumn{5}{c}{" + model_name_line + "} \\\\\n"

	def number_for_sorting(x):
		try:
			return int(x)
		except:
			return x

	model_names = models.keys()
	model_names.sort(key=lambda s: number_for_sorting(s[6]))

	for model in model_names:

		model_name = model
		model_name_lines = model_name.split("NEWLINE")
		model_header = "".join([make_model_name_line(model_name_line) for model_name_line in model_name_lines])

		table_content += "\\hline\n\\hline\n" + model_header

		for portion in ["likelihood", "fixed", "random"]:
			if portion in models[model] and portion != "random":
				if portion == "likelihood":
					line_start = ""#" & "
				else:
					line_start = ""
				data = models[model][portion]

				rows = [dict(data[r]) for r in data.keys()]
				column_names = []
				for row in rows:
					column_names += row.keys()
				column_names = list(set(column_names))
				column_names = sort_columns(column_names)
				column_line = line_start + "& " + " & ".join([cleanup_colname(c) for c in column_names]) + " \\\\"
				
				table_content += column_line + "\n"
				table_content += "\\hline\n"

				row_names = data.keys()
				row_names.sort(key=lambda s: s[0])

				for row_name in row_names:
					row = dict(data[row_name])
					if not (portion=="fixed" and row_name=="(Intercept)"):
						column_values = [get_number(row, column_name) for column_name in column_names]
						content_line = line_start + cleanup_rowname(row_name) + " & " + " & ".join(column_values) + " \\\\"
						table_content += content_line + "\n"

	table_content += "\\hline\n\\hline\n\\end{tabular}\n}"
	#\n\\label{regressions" + study_name + "}\n\\end{table}"

	print table_content
	print
	print
