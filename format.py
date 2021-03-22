import fileinput

for line in fileinput.input():
    x = line.split(" ")

    print(f"""exponents      = {x[1]}
termlimit      = {x[2]}
nonzeroexps    = 10
transfunctions = {x[4]}\n""")
