
const __BASE_URL__ = "http://localhost:8080";

function getUri(path, queryparams) {
    let queries = '';
    if (queryparams) {
        queries = '?' + Object.keys(queryparams)
            .filter(key => queryparams[key] !== undefined)
            .map(key => {
                const value = queryparams[key];
                return `${key}=${value}`;
            }).join('&');
    }

    return encodeURI(__BASE_URL__ + path + queries);
}

module.exports = {
    listTodos: (from, limit, done) => fetch(getUri("/todo", { from: from, limit: limit, done: done }), { method: "GET" }),
    createTodo: (todo) => fetch(getUri("/todo"), { method: 'put', body: JSON.stringify(todo) }),
    updateTodo: (todo) => fetch(getUri("/todo"), { method: 'post', body: JSON.stringify(todo) })
};