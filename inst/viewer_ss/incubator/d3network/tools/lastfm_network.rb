#!/usr/bin/env ruby

# https://github.com/youpy/ruby-lastfm/
require 'lastfm'
require 'json'

api_key = "YOUR_API_KEY"
api_secret = "YOUR_SECRET_KEY"

@lastfm = Lastfm.new(api_key, api_secret)

MAX_CUTTOFF = 12
MATCH_CUTTOFF = 0.07

@ids = []
@current_songs = []

def id_for(song)
  id = [song["name"], song["artist"]].join("_")
  id = id.downcase().gsub(/\s+/,"_").gsub(/\W+/,"")

  # if @ids.include? id
  #   puts "WARNING: already have id for:#{song["name"]}"
  # end
  @ids << id
  id
end
#token = lastfm.auth.get_token

# results = lastfm.track.search("graceland", "Paul Simon")
def get_similar(old_song)
  puts old_song
  begin
  results = @lastfm.track.get_similar(old_song["artist"], old_song["name"])
  rescue Exception => msg
    puts "ERROR: #{msg}"
    results = []
  end
  songs = []
  # puts results.inspect
  results.each do |r|
    match = r["match"].to_f
    if match > MATCH_CUTTOFF
      song = {}
      song["match"] = match
      song["name"] = r["name"]
      song["artist"] = r["artist"]["name"]
      song["id"] = id_for(song)
      song["playcount"] = r["playcount"].to_i
      songs << song
    end
  end
  songs
end

def links_for(origin, songs)
  links = []
  songs.each do |song|
    link = {"source" => origin["id"], "target" => song["id"]}
    reverse_link = {"target" => origin["id"], "source" => song["id"]}
    if !links.include?(link) and !links.include?(reverse_link)
      links << link
    end
  end
  links
end

def unseen_songs(current_songs, new_songs)
  unseen = []
  current_song_ids = current_songs.collect {|cs| cs["id"]}
  new_songs.each do |song|
    if !current_song_ids.include? song["id"]
      unseen << song
    end
  end
  unseen
end

def expand(songs, links, root)
  new_songs = get_similar(root)
  unseen = unseen_songs(songs, new_songs)[0..MAX_CUTTOFF]
  new_links = links_for(root, unseen)
  [unseen, new_links]
end


def grab(root, output_filename)
  links = []
  all_songs = []

  first_iteration, new_links = expand(all_songs, links, root)

  all_songs.concat first_iteration
  links.concat(new_links)

  unlinked_songs = []

  puts all_songs.length
  first_iteration.clone()[1..-1].each do |song|
    puts song["name"]
    new_songs, new_links = expand(all_songs, links, song)
    all_songs.concat(new_songs)
    unlinked_songs.concat(new_songs)
    links.concat(new_links)
    puts all_songs.length
  end

  # second_iteration = all_songs.sample(10)
  # second_iteration.each do |song|
  #   puts song["name"]
  #   new_songs, new_links = expand(all_songs, links, song)
  #   all_songs.concat(new_songs)
  #   unlinked_songs.concat(new_songs)
  #   links.concat(new_links)
  #   puts all_songs.length
  # end

  # song_ids = all_songs.collect {|s| s["id"]}
  # second_iteration = all_songs.sample(20)
  # second_iteration.each do |song|
  #   new_songs = get_similar(song)
  #   new_links = []
  #   new_songs.each do |sim_song|
  #     if song_ids.include?(sim_song["id"]) and sim_song["match"] > MATCH_CUTTOFF
  #       new_links << sim_song
  #     end
  #   end
  #   links.concat(links_for(song, new_links))
  # end


  data = {}
  data["nodes"] = all_songs
  data["links"] = links
  File.open(output_filename, 'w') do |file|
    file.puts JSON.pretty_generate(JSON.parse(data.to_json))
  end
end

roots = [
  # {"name" => "You Can Call Me Al", "artist" => "Paul Simon", "filename" => "call_me_al.json"},
  # {"name" => "Walken", "artist"  => "Wilco", "filename" => "walken.json"},
  # {"name" => "Sledgehammer", "artist"  => "Peter Gabriel", "filename" => "sledgehammer_2_rounds.json"},
  # {"name" => "Ladies Night", "artist"  => "Kool and the gang", "filename" => "ladies_night.json"},
  #{"name" => "Poker Face", "artist"  => "Lady GaGa", "filename" => "poker_face.json"},
  # {"name" => "New Slang", "artist" => "Shins", "filename" => "new_slang.json"},
  # {"name" => "Jolene", "artist" => "Dolly Parton", "filename" => "jolene_2_rounds.json"},
  # {"name" => "January Wedding", "artist" => "Avett Brothers", "filename" => "january_wedding.json"},
  # {"name" => "January Wedding", "artist" => "Avett Brothers", "filename" => "january_wedding.json"},
  # {"name" => "She Said She Said", "artist" => "The Beatles", "filename" => "she_said.json"},
  # {"name" => "Short Skirt Long Jacket", "artist" => "Cake", "filename" => "short_skirt.json"},
  # {"name" => "Good Vibrations", "artist" => "Beach Boys", "filename" => "good_vibrations.json"},
  {"name" => "helplessness blues", "artist" => "Fleet Foxes", "filename" => "helplessness_blues.json"},
]

roots.each do |root|
  root["id"]  = id_for(root)

  grab(root, root["filename"])
end


