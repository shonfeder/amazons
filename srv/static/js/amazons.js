var game_id;

window.onload = _ => {
    game_id = document.querySelector('#game-id')
        .getAttribute('data-game-id');
    add_listeners_to_squares();
};

function add_listeners_to_squares() {
    var squares = document.querySelectorAll('.square');
    for (let sq of squares) {
        sq.addEventListener('click', send_message_of_square);
    }
}

function send_message_of_square(_event) {
    var endpoint = `/api/game/move/${game_id}/${this.id}`;
    var game_container = document.querySelector('.game-container');
    console.log('Game container: ' + game_container);

    console.log('Clicked square: ' + this.id);
    console.log('Sending api call to ' + endpoint);

    fetch(`/api/game/move/${game_id}/${this.id}`)
        .then(response => {
            // TODO: Handle bad response
            if (response.ok) {
                console.log('Response:');
                console.log(response);
                return response.json(); }
            else {
                return illegal_move(response);
            };
        })
        .then(json => {
            if (json) {
                console.log('Json data:');
                console.log(json);
                console.log('Html: ');
                console.log(json.data);
                game_container.innerHTML = json.data;
                add_listeners_to_squares();
            };
        });
}

function illegal_move(data) {
    console.log('Failed.');
    console.log('Response:');
    console.log(data);
    return false;
}
