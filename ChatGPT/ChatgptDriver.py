from openai import OpenAI
import json


def read_api_key(file_path):
    """
    Read the API key from a file.

    :param file_path: Path to the file containing the API key.
    :return: The API key as a string.
    """
    try:
        with open(file_path, 'r') as file:
            return file.read().strip()
    except Exception as e:
        print(f"Error reading the API key: {e}")
        return None
# cache this setting

api_key=read_api_key('api_key.txt')
language = "C"
file_extension = "c"
model = "gpt-4-1106-preview"
seed = 42
# Let's define the prompt and see if it can be reused for other languages

prompt = f"""
    You are an expert software developer and write clean and efficient code.
    I want you to write a Soduko solver in {language}. I will outline each procedure to write:
    and you will write the code. I will test your code and if it passes, I will pay you my respects.
    Step 1: Write a function that reads a 9x9 matrix from a file and returns it as a 1D array,
    please also ignore comments.
        Please do this without the strtok function.
    Step 2: Write a function that calculates the complexity of a 9x9 matrix.
        The complexity is the number of empty cells in the board.
        It is only printed when printing the unsolved board.
    Step 2.1 Write a function that prints the iterations in commas format for thousands.
        using this Example:
        #include <locale.h>
        // Function to format and print the number of iterations
        void printIterationsFormatted(int iterations)
            setlocale(LC_NUMERIC, 'en_US.UTF-8')
            printf('Iterations: %'d', iterations)
    Step 3: Write a function that will print the board in a 9x9 grid. It will take an argument to
    display the calculated complexity or not.
        It should print dividers between the 3x3 squares and the horizontal line spans the width of numbers.
        Add one space between each number.
        Add a Title for unsolved and solved boards.
        Add a newline at the end of the board.
        Make sure that it prints a 0 for empty cells.
    Step 4: Write a function that will solve the Soduko board using a backtracking algorithm.
        Keep track of the number of iterations each time solveSudoku is called.
        add a function prototype for isSafe at the top of the code.
    Step 5: When solved, print the final board and the number of iterations
        formatted with commas for thousands.
        use the locale library to set the locale to one that uses commas as thousands separators,
        such as en_US.UTF-8.
    step 6: The matrices to read will be submitted on the commandline.


    Write the complete code with comments, make sure that it will properly compile and run. Please
    reply with code only in JSON format, no need for addtional information.
    """



client = OpenAI(api_key=api_key,)
messages = [
        {"role": "system", "content": "You are an expert software developer and write clean and efficient code."},
        {"role": "user", "content": prompt},
]

response = client.chat.completions.create(
    model=model,
    messages=messages,
    seed=seed,
    max_tokens=4000,
    temperature=0.7,
    response_format={ "type": "json_object" },
)
response_dict = json.loads(response.choices[0].message.content)

#print(f"{response}")
response_content = response.choices[0].message.content
system_fingerprint = response.system_fingerprint
prompt_tokens = response.usage.prompt_tokens
completion_tokens = (
    response.usage.total_tokens - response.usage.prompt_tokens

)
print(f"Response:{response_content}")
print(f"System Fingerprint:{system_fingerprint}")
print(f"Number of completion tokens:{completion_tokens}")

#save response to a file
file = f"response.{file_extension}"
with open(file, 'w') as file:
    for line in response_dict["code"]:
        file.write(line)





