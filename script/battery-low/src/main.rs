use std::process::Command;
use std::thread;
use std::time::Duration;

fn main() {
    let notification = "\"Low Battery\"\n".to_string();
    loop {
        let output = String::from_utf8(
            Command::new("acpi").arg("-b").output().expect("Read Error").stdout
            ).expect("Parse Error");
        let lst: Vec<&str> = output.split(' ').collect();
        if check_low(&lst) {
            let return_str = notification.clone() + &output;
            Command::new("notify-send")
                .arg("--urgency=critical")
                .arg(return_str)
                .status().expect("Cannot run it");
        }
        thread::sleep(Duration::from_secs(20));
    }
}

fn check_low(lst: &[&str]) -> bool {
    let status = String::from(lst[2]);
    let bat_per: usize = String::from(lst[3]).split("%,").next()
        .unwrap().parse().unwrap();
    match status.split(',').next().unwrap() {
        "Discharging" => bat_per < 15,
        "Full" => false,
        "Charging" => false,
        _ => panic!("Weird Case"),
    }
}
