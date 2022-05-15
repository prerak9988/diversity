# diversity-appsilon
Global Biodiversity and Species Information
Global diversity is an app that let you visualize data - https://drive.google.com/file/d/1l1ymMg-K_xLriFv1b8MgddH851d6n2sU/view?usp=sharing. By using the species identified in individual Family, Kingdom etc. the application offers a graphical representation of the data with maps and charts.


<img width="1792" alt="image" src="https://user-images.githubusercontent.com/29230145/168461529-106a9402-d677-4b58-be5f-ac4c97d6e0bc.png">

<img width="1790" alt="image" src="https://user-images.githubusercontent.com/29230145/168461562-175a57bf-28c6-4131-a66a-139629955155.png">


Links:

Shinyapps: https://product-redesign-tool.shinyapps.io/appsilon-hiring/

Github:

Features:

The application layout is made with [Shiny, Shinythemes and Shinydashboard] - css and sass are used to customise the app

The maps are realized with the [leaflet] and timeline with [timevis]

A visual representation of the species Tree charts is made with the [collapsibleTree] package, which makes the graphical representation of hierarchical trees very effective

Serverside rendered user inputs are used - PickerInput, SelectizeInput, selectInput as the data set is huge

[rPivotTable] is used to create grouped dynamic views to visualise multiple metrics at same time
