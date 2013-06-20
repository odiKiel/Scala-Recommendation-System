Source code for the diplom thesis
======

The package name is odi.recommendation
The source can be found in src/main/scala/odi/recommendation.
All files from the public folder are accessible for external clients.
These files are the HTML and JavaScript files.

For this system the Scala simple build tool should be used.
Start the system: 
Start sbt in the console by typing 'sbt'.
Run the scala console by typing 'console'.
Import the package by typing: import odi.recommendation._
Create all tables by typing: TestDatabase.initialSetup
Load test data by typing: TestDatabase.setup
Activate the WebService by typing 'WebService(10000)'.

The system is now accessible on http://localhost:10000.
The test environment for the tagging system can be accessed on http://localhost:10000/tagger.
The recommendation example can be started on http://localhost:10000/recommend.

After the askbot question and answer site is installed the question.html page must be altered.
The question.html file can be found in the following directory:
/askbot/skins/default/templates  

The following lines should be added to the javascript container:

        var _rating = _rating || {};
        _rating['user'] = "{{user.id}}";
        _rating['item'] = "{{question.id}}";
        _rating['button'] = ["#add-answer-btn", ".post-vote"];
        _rating['information'] = ".question-body>p";
        _rating['title'] = "h1";

<script src="localhost:10000/file/js/rating/rating.js"></script>       


For more questions please contact odi@informatik.uni-kiel.de.
