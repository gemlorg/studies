def delete_lines_not_divisible_by_8(file_path):
    try:
        with open(file_path, 'r') as file:
            lines = file.readlines()

        # Filter lines based on line numbers divisible by 8
        filtered_lines = [line for i,
                          line in enumerate(lines, 1) if (i+1) % 8 == 1]

        with open(file_path, 'w') as file:
            file.writelines(filtered_lines)

        print(f"Lines not divisible by 8 removed from {file_path}")

    except FileNotFoundError:
        print(f"File '{file_path}' not found.")
    except Exception as e:
        print(f"An error occurred: {e}")


# Example usage: Specify the path to your file
file_path = './data.txt'
delete_lines_not_divisible_by_8(file_path)
