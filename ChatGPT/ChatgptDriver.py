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

client = OpenAI(api_key=read_api_key('api_key.txt'),)

language = "Python"
model = "gpt-4-1106-preview"
seed = 42

prompt = f"""
    You are an expert software developer and write clean and efficient code.
    I want you to write a Soduko solver in {language}. I will outline each procedure to write:
    and you will write the code. I will test your code and if it passes, I will pay you my respects.
    Step 1: Write a function that reads a 9x9 matrix from a file and returns it as a 1D array,
    please also ignore comments.
    Step 2: Write a function that calculates the complexity of a 9x9 matrix.
    Step 3: Write a function that will print the board in a 9x9 grid. It will take an argument to
    display the calculated complexity or not.
    Step 4: Write a function that will solve the Soduko board using a backtracking algorithm.
    Step 5: When solved print the final board and the number of iterations.
    step 6: The matrices to read will be submitted on the commandline.

    Write the complete code with comments, make sure that it will properly compile and run. Please
    reply with code only in JSON format, no need for addtional information.
    """


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
dict = json.loads(response.choices[0].message.content)

#print(response)
#print(response.choices[0].message.content)


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
with open('response.py', 'w') as file:
    for line in dict["code"]:
        file.write(line)





