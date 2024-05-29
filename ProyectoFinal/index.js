import data from './Persons.json' assert {type: 'json'};
async function getPersonas() {
    console.log(data)
}
// const Personas = 
getPersonas()
data.forEach(element => {
    document.getElementById('personas').innerHTML += '<br>' + JSON.stringify(element);
});