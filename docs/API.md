# Unauthenticated Read Operations #

## GET /apps ##
Returns an `apps` list containing the `name` and `description` of applications that have achievements.

Example output:
````
{"apps":[{"name":"Nethack","description":"A text-based, turn-based, dungeon-crawling RPG"},{"name":"CSH GameJam 2011","description":"Computer Science House GameJam Winter 2011"},{"name":"netrek","description":"Paradise Netrek @ whitestar.cs.rit.edu"}]}
````

## GET /apps/:appName ##
Returns a list of `achievements` that are contained within this application.

* `id` = A guaranteed-unique number ID number that refers to this achievement. This ID number is global to all achievements, not just achievements for a particular application. This is mostly included for backwards compatibility with existing achievements using applications, which previously had to specify know this number for updates.
* `title` = A small (<=80 characters, mostly much less) witty title for this achievement.
* `description` = A <=255 character description of the achievement.
* `score` = How much this achievement is worth when completed.

Example output:
````
GET /apps/nethack
{"achievements":[{"id":1,"title":"Titillating Tech Talk","description":"Chat with Andy Potter","score":5},{"id":2,"title":"Neither Rain nor Sleet nor Snow","description":"Recieve a scroll from the mail daemon","score":5}]}
````

## Get /apps/:appName/events ##
Returns a list of `events` (aka 'binges') that this application is currently having or has had. You could use this information to create some sort of live scoreboard, for instance, or for calendar / dropdown purposes.

Time is expressed in seconds since the UNIX epoch.

Example output:
````
GET /apps/nethack/events
{"events":[{"app name":"Nethack","event title":"Independence Day Full Moon","start time":1341324000,"end time":1341367200},{"app name":"Nethack","event title":"Winter Quarter New Moon","start time":1357992000,"end time":1358078400}]}
````

## GET /apps/:appName/users ##
Returns a list of `users` that have achievement progress in this application.

````
GET /apps/nethack/users
{"users":[{"username":"clockfort"},{"username":"russ"}]}
````

## GET /apps/:appName/users/:username ##

Returns a list of achievements that this user has in this application.

The user "has" the achievement if user progress = max progress. I'll probably add a "completed" tag to make things easier for API users at some point.


````
{"achievements":[{"app name":"Nethack","title":"Developers, Developers, Developers, Developers","description":"Make a non-trivial source commit","max progress":1,"user progress":1,"score":10,"updated at":1315633538}]}
````

## GET /users ##

Get a global list of `users` (which just contain `username`s).

## GET /events ##

Get a global list of `events`.

## GET /users/:userName ##

Get a list of all a user's achievments, in all applications.
