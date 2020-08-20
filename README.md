# regional_workflow

# Build and install the regional workflow
1. Check out the regional workflow external components:

`cd sorc`

`./manage_externals/checkout_externals`

2. Build the ARW executables:

`cd arw`

`./link_fix.sh`

The link_fix.sh step also pulls in prebuilt libraries needed for the build.

`./build_hiresw.sh`

3. Install the ARW executables:

`./install_hiresw.sh`

4. Build the FV3 executables:

`cd ../fv3`

`./link_fix.sh`

The link_fix.sh step also pulls in prebuilt libraries needed for the build.

`./build_all.sh`

5. Install the FV3 executables:

`./install_all.sh`
