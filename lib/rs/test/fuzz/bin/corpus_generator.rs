// Licensed to the Apache Software Foundation (ASF) under one
// or more contributor license agreements. See the NOTICE file
// distributed with this work for additional information
// regarding copyright ownership. The ASF licenses this file
// to you under the Apache License, Version 2.0 (the
// "License"); you may not use this file except in compliance
// with the License. You may obtain a copy of the License at
//
//   http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing,
// software distributed under the License is distributed on an
// "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
// KIND, either express or implied. See the License for the
// specific language governing permissions and limitations
// under the License.

use arbitrary::{Arbitrary, Unstructured};
use clap::Parser;
use std::fs::{self, File};
use std::io::Read;
use std::path::Path;
use thrift::protocol::{
    TBinaryOutputProtocol, TCompactOutputProtocol, TOutputProtocol, TSerializable,
};
use thrift::transport::TBufferChannel;
use thrift_fuzz::fuzz_test::FuzzTest;

#[derive(Parser)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// Input directory containing raw binary files (mutually exclusive with --generate)
    #[arg(short, long, group = "input")]
    input_dir: Option<String>,

    /// Number of random files to generate (mutually exclusive with --input-dir)
    #[arg(short, long, group = "input")]
    generate: Option<usize>,

    /// Output directory for serialized FuzzTest files
    #[arg(short, long)]
    output_dir: String,

    /// Protocol to use for serialization (binary or compact)
    #[arg(short, long)]
    protocol: String,

    /// Buffer size for serialization (default: 65536)
    #[arg(short, long, default_value = "65536")]
    buffer_size: usize,

    /// Size of random byte vector for generation (default: 16384)
    #[arg(long, default_value = "16384")]
    random_size: usize,
}

fn serialize_fuzz_test(
    fuzz_test: &FuzzTest,
    protocol: &str,
    buffer_size: usize,
) -> Result<Vec<u8>, Box<dyn std::error::Error>> {
    let mut mem = TBufferChannel::with_capacity(buffer_size, buffer_size);
    match protocol {
        "binary" => {
            let mut out_protocol = TBinaryOutputProtocol::new(&mut mem, true);
            fuzz_test.write_to_out_protocol(&mut out_protocol)?;
            out_protocol.flush()?;
        }
        "compact" => {
            let mut out_protocol = TCompactOutputProtocol::new(&mut mem);
            fuzz_test.write_to_out_protocol(&mut out_protocol)?;
            out_protocol.flush()?;
        }
        _ => return Err("Invalid protocol specified. Use 'binary' or 'compact'".into()),
    }
    Ok(mem.write_bytes().to_vec())
}

fn convert_corpus_file(
    input_path: &Path,
    output_dir: &Path,
    protocol: &str,
    buffer_size: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    // Read input file
    let mut input_file = File::open(input_path)?;
    let mut input_data = Vec::new();
    input_file.read_to_end(&mut input_data)?;

    // Create Unstructured instance for arbitrary
    let mut unstructured = Unstructured::new(&input_data);

    // Generate FuzzTest instance
    if let Ok(fuzz_test) = FuzzTest::arbitrary(&mut unstructured) {
        // Create output file path
        let file_name = input_path
            .file_name()
            .ok_or("Invalid input filename")?
            .to_str()
            .ok_or("Invalid UTF-8 in filename")?;
        let output_path = output_dir.join(file_name);

        // Serialize and write to file
        let serialized_data = serialize_fuzz_test(&fuzz_test, protocol, buffer_size)?;
        fs::write(output_path, serialized_data)?;
    }

    Ok(())
}

fn generate_random_file(
    output_dir: &Path,
    index: usize,
    protocol: &str,
    buffer_size: usize,
    random_size: usize,
) -> Result<(), Box<dyn std::error::Error>> {
    // Generate random bytes
    let random_bytes: Vec<u8> = (0..random_size).map(|_| rand::random::<u8>()).collect();

    // Create Unstructured instance for arbitrary
    let mut unstructured = Unstructured::new(&random_bytes);

    // Generate FuzzTest instance
    if let Ok(fuzz_test) = FuzzTest::arbitrary(&mut unstructured) {
        // Create output file path with index
        let output_path = output_dir.join(format!("generated_{index}.bin"));

        // Serialize and write to file
        let serialized_data = serialize_fuzz_test(&fuzz_test, protocol, buffer_size)?;
        fs::write(output_path, serialized_data)?;
    }

    Ok(())
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let args = Args::parse();

    // Validate protocol
    if args.protocol != "binary" && args.protocol != "compact" {
        return Err("Invalid protocol specified. Use 'binary' or 'compact'".into());
    }

    // Create output directory if it doesn't exist
    fs::create_dir_all(&args.output_dir)?;

    match (args.input_dir, args.generate) {
        (Some(input_dir), None) => {
            // Process each file in the input directory
            for entry in fs::read_dir(&input_dir)? {
                let entry = entry?;
                let path = entry.path();
                if path.is_file() {
                    if let Err(e) = convert_corpus_file(
                        &path,
                        Path::new(&args.output_dir),
                        &args.protocol,
                        args.buffer_size,
                    ) {
                        eprintln!("Error processing file {path:?}: {e}");
                    }
                }
            }
        }
        (None, Some(num_files)) => {
            // Generate random files
            for i in 0..num_files {
                if let Err(e) = generate_random_file(
                    Path::new(&args.output_dir),
                    i,
                    &args.protocol,
                    args.buffer_size,
                    args.random_size,
                ) {
                    eprintln!("Error generating file {i}: {e}");
                }
            }
        }
        _ => return Err("Must specify either --input-dir or --generate".into()),
    }

    Ok(())
}
