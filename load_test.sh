while [ 1 ]; do
    time curl "http://localhost:3000/cloud/generate-answers?semantics=%7B%22synsem%22%3A%7B%22sem%22%3A%7B%22aspect%22%3A%22progressive%22%2C%22subj%22%3A%7B%22mass%22%3Afalse%2C%22furniture%22%3Afalse%2C%22pred%22%3A%22lei%22%2C%22place%22%3Afalse%2C%22drinkable%22%3Afalse%2C%22human%22%3Atrue%2C%22animate%22%3Atrue%2C%22speakable%22%3Afalse%2C%22activity%22%3Afalse%2C%22physical-object%22%3Atrue%2C%22buyable%22%3Afalse%2C%22legible%22%3Afalse%2C%22artifact%22%3Afalse%2C%22edible%22%3Afalse%2C%22part-of-human-body%22%3Afalse%7D%2C%22tense%22%3A%22present%22%2C%22discrete%22%3Afalse%2C%22pred%22%3A%22dormire%22%7D%2C%22subcat%22%3A%5B%5D%2C%22cat%22%3A%22verb%22%7D%7D&_=1417843915349" | jq '.'
    sleep 2
done
