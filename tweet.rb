require 'twitter'

# Capture a path to the jpeg
jpg_path = ARGV[0]

# Choose a random message
message = [
  'Yes.',
  'You better believe it!',
  'Was there any doubt?',
  'Gave \'em hell!',
  'Boddagetta boddagetta boddagetta bah!',
  'Stand up and yell, hey!',
  'Rah rah rah, sisk boom bah!',
  'Weagle weagle!',
  'Strike up the band!',
  'BOOM!'
  ].sample + ' #WarEagle'

# store development keys in gitignored files
if File.exists?('.twitter_consumer_secret')
  CONSUMER_SECRET = File.read('.twitter_consumer_secret').strip
  ACCESS_SECRET = File.read('.twitter_access_secret').strip
# store production keys in environment variables
else
  CONSUMER_SECRET = ENV['TWITTER_CONSUMER_SECRET']
  ACCESS_SECRET = ENV['TWITTER_ACCESS_SECRET']
end

# wire up Twitter using keys and secrets
client = Twitter::REST::Client.new(
  :consumer_key       => 'go0ZzbprJtz5O65Wjlej0KIjT',
  :consumer_secret    => CONSUMER_SECRET,
  :access_token        => '3048458734-HQAB5Ng7D13REDNvWS5PX7FAlGExD0hcfwUiSii',
  :access_token_secret => ACCESS_SECRET,
)

client.update_with_media message, File.new(jpg_path)