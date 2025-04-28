# function to create table of essay answers in study 3
create_table_essay_answers_study3 <- function() {
  tibble(
    Answer = c("Father", "Sister", "Friend",
               "The Hobbit", "Buffy the Vampire Slayer", "Titanic"),
    Text = c(
      
      # Father

      "My dad is one of the most important people in my life. He's always been
      someone I look up to and rely on. Throughout my whole life, he's been
      there to guide me, teach me, and support me in everything I do. What
      makes my dad special is how much he cares about our family. He works hard
      every day to make sure we have what we need, but no matter how busy he is,
      he always makes time for us. My dad is emotionally strong. Even though
      he doesn't show his emotions a lot, I can tell how much he cares by how
      much he does for us. When things get hard, he stays calm and steady, and
      that helps me feel better. One of my favorite things about my dad is
      how much he loves to teach. He knows so much and is always happy to share
      what he knows. He explains things in a way that makes sense and is easy to
      understand. I also love my dad's sense of humor. He always knows how to
      make me laugh with a joke or a funny story. His laughter makes everything
      feel lighter and happier. My dad has taught me so much about working
      hard, being kind, and staying strong when life is tough. I'm so thankful
      for everything he's done for me, and I'm proud to have him as my dad!",

      # Sister

      "My sister is one of the most important people in my life. She is special
      because she always supports me. She has a way of making me feel confident,
      even when I'm unsure of myself. Whenever I'm scared to try something new,
      she's the first to remind me of what I can do. Her belief in me helps me
      believe in myself. My sister also has a really kind heart. She always
      thinks about others and does her best to help. She's always putting others
      first, whether it's being there for a friend or helping out with family.
      Her kindness is something I look up to and try to follow. Another thing
      I love about my sister is how funny she is. She has a great sense of humor
      and always knows how to make people laugh, even in serious moments. If I'm
      ever feeling down, she can cheer me up with a joke or a funny story. Her
      laughter makes everything feel lighter and happier. What I admire most
      about my sister is how strong she is. She's faced tough times but never
      lets them hold her back. Her strength gives me courage to keep going when
      life gets hard. My sister is more than just a family member — she's my
      role model and my rock!",

      # Best friend

      "My best friend is one of the most amazing people I know. She's someone I
      can count on no matter what. What makes her so special is her kindness.
      She always makes people feel important and cared for. Whether it's helping
      someone she just met or being there for her friends, she's the first to
      offer support. She never hesitates to help me, whether I'm upset or just
      having a bad day. She also has a great sense of humor that can cheer
      anyone up. She finds ways to laugh about even the smallest things, and her
      laugh is so contagious! Her laughter makes everything feel lighter and
      happier. What I admire most about her is how strong she is. Life hasn't
      always been easy for her, but she never gives up. She stays calm and keeps
      going, no matter what happens. Watching her face challenges in adulthood
      has taught me to be brave and not let hard times hold me back. My best
      friend has shown me what it means to be loyal, caring, and strong. I feel
      so lucky to have her in my life. I try to be as good of a friend to her as
      she is to me. She inspires me to be a better person!",

      # The Hobbit

      "I will focus on describing the book “The Hobbit” by Tolkien. The Hobbit
      is a fantasy adventure story about Bilbo Baggins. Bilbo is a quiet hobbit
      who lives in the Shire. His life changes when Gandalf the wizard and a
      company of dwarves ask him to join their quest to take back treasure
      stolen by a dragon. At the beginning of the journey, Bilbo and the
      dwarves are nearly eaten by trolls, but Gandalf saves them. Then later, in
      the Misty Mountains, Bilbo meets a creature called Gollum and finds a
      magical ring that makes him invisible. This ring later becomes very
      important in “The Lord of the Rings”. As they travel, the group fights
      goblins, giant spiders in Mirkwood forest, and they get captured by
      Wood-elves. Bilbo shows his bravery by saving the group several times.
      Finally, they reach the Lonely Mountain where the dragon Smaug lives.
      Bilbo sneaks into the dragon's lair and finds a weak spot in Smaug's
      armor. The dragon gets angry and attacks the nearby town by a lake.
      Eventually, Smaug is killed. With the dragon dead, humans, elves, and
      dwarves all want the treasure. This leads to the “Battle of the Five
      Armies”. Tolkien doesn't describe the battle in too much detail, but we
      later learn that the leader of the dwarves Thorin has fought bravely and
      died from his wounds. At the end of the story, Bilbo returns home to
      the Shire, richer and wiser from his adventure. He is happy to be back in
      his quiet life, and sets out to write a book of his adventures - which
      sets the stage for the sequel, The Lord of the Rings.",

      # Buffy the Vampire Slayer

      "I will focus on describing the TV show “Buffy the Vampire Slayer”. Buffy
      is a TV show that completely flips the script on traditional high school
      dramas and supernatural horror. It's about a teenager, Buffy Summers,
      who's tasked with being the Slayer – basically a chosen one who hunts
      vampires and other demons. But what sets the show apart is how Buffy
      struggles to balance her responsibility with the regular teenage
      experience. She's not just fighting creatures of the night, she's also
      balancing school and friendships at the same time. One of the most
      striking things about Buffy is how layered the characters are. Buffy is
      tough and witty, but she's also vulnerable. She's faced with loss, guilt,
      and trying to make sense of her life outside of the supernatural chaos.
      And then there's her team. Willow is the nerdy, sweet heart of the group,
      Xander is the funny loyal friend, and Giles (Buffy's Watcher) is the stern
      mentor who's also loving. Each character feels real, with their own flaws
      and growth arcs. The show has this incredible ability to mix humor,
      heart, and horror seamlessly. The dialogue is sharp and full of clever pop
      culture references. Yet, the writing isn't afraid to get serious,
      exploring themes like trauma and growing up. The monsters Buffy faces
      often mirror real-life challenges, making the stakes feel personal. I
      love Buffy. It's a show that's smart and emotional, blending witty banter
      with moments of real depth. It's got a cult following for a reason!",

      # Titanic

      "I will focus on describing the film “Titanic”. The genre is a mix of
      romance, disaster, and historical tragedy. The film tells the love story
      of Jack and Rose, two passengers from different social classes aboard the
      passenger ship Titanic. Jack is a poor artist, but he manages to win a
      ticket to the ship's maiden voyage. Rose is a young upper-class woman who
      is feeling trapped in her engagement to her fiance. Jack and Rose cross
      paths on the ship, and they fall in love. The film balances the
      spectacle of the ship's design and atmosphere with the tension that
      gradually builds as the audience knows what fate awaits. The Titanic sails
      into the icy waters of the Atlantic and strikes an iceberg. Chaos
      immediately erupts. The film allows viewers to experience the terror,
      confusion, and heartbreak of the tragedy, showcasing both personal stories
      and the broader catastrophe. At its core, the film is a romance. But
      Titanic also touches on themes of class and fate. It highlights the
      disparity between the elite and the working-class passengers who are
      doomed to different fates. The film also explores the sense of
      inevitability that comes with knowing the ship's doom. The most iconic
      scene from the film is arguably the scene where Jack and Rose stand
      together at the bow, arms outstretched. They seem free, but the scene also
      foreshadows the devastating crash to come. The film is truly
      heartbreaking!"
    )
  ) %>%
    mutate(Text = str_replace_all(Text, fixed("\n      "), " "))
}
