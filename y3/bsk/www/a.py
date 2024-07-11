def scolarship(name, grades):
    return (not None in grades) and (sum(grades) / len(grades)) > 75 and min(grades) >= 60
