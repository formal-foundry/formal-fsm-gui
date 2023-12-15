# fsm-agda 
Finite State Machine - Agda 

Program Installation and Configuration Guide
Prerequisites
Before proceeding with the installation, ensure that you have the following prerequisites installed on your system:

Git,
Stack


Step 1: Clone and Run the Program
Clone the program repository from the following link: https://github.com/nieradkokrystian/agda-typecheck-web,
navigate to the cloned repository and read README instruction.

Step 2: Create Necessary Directories and Configuration Files
Create a directory to store essential files. For example, run the command:

bash
* mkdir ~/.fsm *

Copy the contents of the 'to_copy_out' directory from the cloned repository to the newly created directory:

bash
* cp -r path/to/cloned/repository/to_copy_out/* ~/.fsm/ *

Step 3: Set Up Working Directory
Create a working directory where all necessary files will be generated.

* mkdir path/to/working_directory *

  Step 4: Edit Configuration File
Edit the config.json file located at ~/.fsm.
Modify the necessary parameters according to your requirements.

Step 5: Build the Repository
Build the repository using the following command:

bash
* stack build *

  Step 6: Run the Program
Launch the program with the following command:

bash
* stack run *


Step 7: Access the Web Interface
If you haven't changed the port in the configuration file, the web interface will be available at:
http://localhost:8011/index.html




In case of any questions or doubts, please write to krystian.nieradko@gmail.com.

