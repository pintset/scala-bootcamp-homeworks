# dotnet installation
export DEBIAN_FRONTEND=noninteractive
apt update
apt install -y ca-certificates wget
apt update
wget https://packages.microsoft.com/config/ubuntu/18.04/packages-microsoft-prod.deb -O packages-microsoft-prod.deb
dpkg -i packages-microsoft-prod.deb
rm packages-microsoft-prod.deb
apt install -y apt-transport-https
apt update && apt install -y dotnet-sdk-5.0

# Building
dotnet publish ./Calculator.fsproj -c Release -o ./release