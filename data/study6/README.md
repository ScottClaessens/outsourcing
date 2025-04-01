# Data README file

**Dataset title:** Clean data from Study 6 for project on "Social
Outsourcing and AI"

**Principal investigator:** Dr. Scott Claessens (scott.claessens@gmail.com)

**Head researcher:** Dr. Jim Everett (j.a.c.everett@kent.ac.uk)

**Institution:** University of Kent

**File format:** CSV file

**File dimensions:** 651 rows x 22 columns

**Data collected on:** 1st April 2025

**Columns in the dataset:**

- `id` - numeric, participant identification number
- `treatment` - character, randomly assigned treatment
- `captcha` - numeric, 0-1, Captcha score from Qualtrics
- `attention` - character, response to the attention check question. The
question was "When an important event is happening or is about to happen, many 
people try to get informed about the development of the situation. In such 
situations, where do you get your information from?" On the previous page,
participants are asked to respond to this question by saying "TikTok"
- `age` - numeric, participant's reported age in years
- `gender` - character, participant's self-reported gender identity
- `comprehension1` - character, response to the first comprehension question.
The question was "In the scenario, what did Adam write?" The correct answer is
"A love letter"
- `comprehension2` - character, response to the second comprehension question.
The question was "In the scenario, did Adam use AI to help write the love 
letter?" The correct answer is "No" for the Control treatment group and "Yes"
for the Tool and Full treatment groups
- `effort` - numeric, 1-7 Likert, response to the question "How much effort do
you think Adam put into the love letter?" ranging from None At All (1) to Very
Much (7)
- `authentic` - numeric, 1-7 Likert, response to the question "How authentic do
you think Adam's love letter was?" ranging from Not At All (1) to Very Much (7)
- `care` - numeric, 1-7 Likert, response to the question "How much do you think
Adam cares about the love letter?" ranging from Not At All (1) to Very Much (7)
- `competent` - numeric, 1-7 Likert, response to the question "How well do each
of the following words describe Adam? Competent" ranging from Does Not Describe
Adam Well (1) to Describes Adam Extremely Well (7)
- `warm` - numeric, 1-7 Likert, response to the question "How well do each of
the following words describe Adam? Warm" ranging from Does Not Describe Adam 
Well (1) to Describes Adam Extremely Well (7)
- `moral` - numeric, 1-7 Likert, response to the question "How well do each of 
the following words describe Adam? Moral" ranging from Does Not Describe Adam 
Well (1) to Describes Adam Extremely Well (7)
- `lazy` - numeric, 1-7 Likert, response to the question "How well do each of 
the following words describe Adam? Lazy" ranging from Does Not Describe Adam 
Well (1) to Describes Adam Extremely Well (7)
- `trustworthy` - numeric, 1-7 Likert, response to the question "How well do 
each of the following words describe Adam? Trustworthy" ranging from Does Not 
Describe Adam Well (1) to Describes Adam Extremely Well (7)
- `openended1` - character, open ended response to the question "In your own
words, describe how you feel about Adam and why"
- `openended2` - character, open ended response to the question "In your own
words, explain how you would feel if you were Adam's partner"
- `chatgpt_familiarity` - numeric, 1-7 Likert, response to the question "How
familiar are you with AI tools like ChatGPT?" ranging from Extremely Unfamiliar
(1) to Extremely Familiar (7)
- `chatgpt_used` - character, response to the question "Have you ever used
AI tools like ChatGPT before?", Yes/No response
- `chatgpt_frequency` - numeric, 1-5 Likert, response to the question "How 
frequently do you use AI tools like ChatGPT?" ranging from Never (1) to Very 
Frequently (5)
- `chatgpt_trust` - numeric, 1-7 Likert, response to the question "How
trustworthy do you think AI tools like ChatGPT are?" ranging from Extremely 
Untrustworthy (1) to Extremely Trustworthy (7)
