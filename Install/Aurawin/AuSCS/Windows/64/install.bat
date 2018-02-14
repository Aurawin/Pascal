@echo "Verifying Aurawin Folders"
mkdir "C:\Program Files\Aurawin"
mkdir "C:\Program Files\Aurawin\SCS"
mkdir "C:\Program Files\Aurawin\SCS\Logs"

@echo "Verifying AuraDisks..."
mkdir "C:\AuraDisks"

@echo "Copying Files..."

copy .\AuProcess.exe "C:\Program Files\Aurawin\SCS\"
copy .\AuService.exe "C:\Program Files\Aurawin\SCS\"
copy .\remove.bat "C:\Program Files\Aurawin\SCS\"
copy .\Readme.txt "C:\Program Files\Aurawin\SCS\"

"C:\Program Files\Aurawin\SCS\AuService" --install"
@echo "Aurawin Social Computing Server Installed."
