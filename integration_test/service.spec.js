
require('isomorphic-fetch');

const service = require("./service");

async function getLatestTodo() {
    const getResult = await service.listTodos(0, 1);

    expect(getResult.status).toEqual(200);

    return (await getResult.json())[0];
}

describe('todo tests', () => {
    test('create and update', async () => {
        const description = 'e2e test todo' + (new Date().getTime());
        const size = "l";
        const createResult = await service.createTodo({
            description: description,
            size: size
        });

        expect(createResult.status).toEqual(200);

        const id = await createResult.json();

        const saved = await getLatestTodo();

        expect(saved.description).toEqual(description);
        expect(saved.size).toEqual(size);
        expect(saved.done).toEqual(false);

        const newDescription = 'new description' + (new Date().getTime());

        const newSize = "s";

        const updateResult = await service.updateTodo({
            id: id,
            done: true,
            description: newDescription, 
            size: newSize
        });

        expect(updateResult.status).toEqual(200);

        const updated = await getLatestTodo();

        expect(updated.description).toEqual(newDescription);
        expect(updated.size).toEqual(newSize);
        expect(updated.done).toEqual(true);
    });

    test('lists only done todos', async () => {

        const listResult = await service.listTodos(0, 100, true);

        expect(listResult.status).toEqual(200);

        const result = await listResult.json();

        expect(result.every((todo) => todo.done === true)).toEqual(true);
    });

    test('lists only not done todos', async () => {

        const listResult = await service.listTodos(0, 100, false);

        expect(listResult.status).toEqual(200);

        const result = await listResult.json();

        expect(result.every((todo) => todo.done === true)).toEqual(false);
    });

});