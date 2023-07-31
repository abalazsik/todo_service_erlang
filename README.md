todo_service_erlang
====

It turns out, there is not much information on the web about how, to write, and test restful applications. So here is an example application that can give you a good idea about these topics.

In this example REST application you can see:  
- how to test the backend code with eunit & meck  
- how to use odbc with postgres  
- how to run it in docker compose  


Start the application
----

```
docker-compose -f docker-compose.yml up
```

running the integrations tests (from the integration_test folder)
```
npm run test
```

Generate coverage report
----
```
rebar3 eunit --cover && rebar3 cover --verbose
```