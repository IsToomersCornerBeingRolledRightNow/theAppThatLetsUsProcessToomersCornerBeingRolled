# -*- mode: ruby -*-
# vi: set ft=ruby :

Vagrant.configure("2") do |config|
  config.vm.box      = "ubuntu_trusty64"
  config.vm.box_url  = "http://cloud-images.ubuntu.com/vagrant/trusty/current/trusty-server-cloudimg-amd64-vagrant-disk1.box"
  config.vm.hostname = "istoomersrolled"

  config.vm.provision "shell" do |s|
    s.path = "bootstrap"
  end

  config.vm.provider "virtualbox" do |v|
    v.memory = 1024
  end
end
