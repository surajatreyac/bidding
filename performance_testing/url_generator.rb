require 'securerandom'

base_url = ARGV[0] || 'http://localhost:8080'
prefix = ARGV[1] || ''
amount = (ARGV[2] || 100).to_f

$stderr.puts amount

connection_types = ['edge', '3G', 'WiFi', 'WiFi', 'WiFi', 'WiFi']
bundle_name = ['com.whatsapp', 'com.facebook', 'com.rdio', 'com.spotify', 'com.twitter', 'com.slack', 'com.rovio.angry_birds']
ips = IO.readlines('ips.txt').map(&:chomp)
uuids = IO.readlines('uuids.txt').first.split(',')

urls = []
(amount * 0.7).to_i.times do
  urls << "#{prefix} #{base_url}/bid_request?auction_id=#{uuids.pop}&ip=#{ips.sample}&bundle_name=#{bundle_name.sample}&connection_type=#{connection_types.sample}"
end

uuids = IO.readlines('uuids.txt').first.split(',').shuffle!(random: SecureRandom.random_number(100))
(amount * 0.3).to_i.times do
  urls << "#{prefix} #{base_url}/impression?auction_id=#{uuids.pop}"
end


filename = "urls-#{Time.now.utc.to_i}.txt"
File.write("./#{filename}", urls.join("\n"))

#vegeta -cpus=4 attack -targets=urls.txt -workers=10 | tee results.bin | vegeta report

