from openai import OpenAI
import json


def read_return_file(file_path):
    """
    Read the API key from a file.

    :param file_path: Path to the file containing the API key.
    :return: The API key as a string.
    """
    try:
        with open(file_path, 'r') as file:
            return file.read().strip()
    except Exception as e:
        print(f"Error reading the file: {e}")
        return None
# cache this setting

api_key=read_return_file('api_key.txt')
prompt=read_return_file('prompt.txt')

language = "java"
file_extension = "java"
model = "gpt-4-1106-preview"
seed = 42
# Let's define the default prompt if prompt.txt and see if it can be reused for other languages
if prompt is None:
    prompt = f"""
    You are an expert software developer and write clean and efficient code.
    I want you to write a Soduko solver in {language}. I will outline each procedure to write:
    and you will write the code. I will test your code and if it passes, I will pay you my respects.
    Please add comments for each step below.
    Make sure the code you provede compiles without errors or warnings.
    Step 1: Write a function that reads a 9x9 matrix from a file and returns it as a 1D array,
    please also ignore comments.
        Print the board after it is read and when it is solved.
    Step 2: Write a function that calculates the complexity of a 9x9 matrix.
    Step 3: Write a function that will print the board in a 9x9 grid.
        Print the complexity only for the unsolved board
    Step 4: Write a function that will solve the Soduko board using a backtracking algorithm.
    Step 5: When solved print the final board and the number of iterations, using commas for thousands.
    step 6: The matrices to read will be submitted on the commandline.
    Check the 3x3 square for the number, before backtracking
    Please follow each step slowly and carefully, I will be checking your work.
    Make sure to print the  complexity and the number of iterations.
    Write the complete code with comments, make sure that it will properly compile and run. Please
    reply with code only in JSON format, no need for addtional information.

    """
# evaluate language variable
prompt = prompt.format(language=language)

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
file = "response.{file_extension}".format(file_extension=file_extension)

with open(file, 'w') as file:
    for line in response_dict["code"]:
        file.write(line)





