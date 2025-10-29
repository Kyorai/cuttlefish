# Changelog

## [v3.6.0](https://github.com/Kyorai/cuttlefish/tree/v3.6.0) (2025-10-29)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v3.5.0...v3.6.0)

**Closed issues:**

- Support multi-line values [\#60](https://github.com/Kyorai/cuttlefish/issues/60)
- 4.0.0 [\#55](https://github.com/Kyorai/cuttlefish/issues/55)
- Improve error reporting when an unsupported option is provided [\#44](https://github.com/Kyorai/cuttlefish/issues/44)
- Update EDoc usage [\#36](https://github.com/Kyorai/cuttlefish/issues/36)
- Consider detecting Unicode BOM marks in conf files [\#32](https://github.com/Kyorai/cuttlefish/issues/32)

**Merged pull requests:**

- Provide more informative output [\#66](https://github.com/Kyorai/cuttlefish/pull/66) ([lukebakken](https://github.com/lukebakken))
- Fix edoc warning [\#65](https://github.com/Kyorai/cuttlefish/pull/65) ([lukebakken](https://github.com/lukebakken))
- Handle conf files that start with unicode BOM [\#64](https://github.com/Kyorai/cuttlefish/pull/64) ([lukebakken](https://github.com/lukebakken))
- Support multiline configuration values [\#63](https://github.com/Kyorai/cuttlefish/pull/63) ([lukebakken](https://github.com/lukebakken))
- GitHub actions workflow updates [\#61](https://github.com/Kyorai/cuttlefish/pull/61) ([lukebakken](https://github.com/lukebakken))

## [v3.5.0](https://github.com/Kyorai/cuttlefish/tree/v3.5.0) (2025-08-11)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v3.4.0...v3.5.0)

**Closed issues:**

- Colons in generated values are not parsed the way the user expects them to be [\#56](https://github.com/Kyorai/cuttlefish/issues/56)
- \[bug\] Tag 2.0.1 conflicts with basho repo, obstructing synchronization [\#54](https://github.com/Kyorai/cuttlefish/issues/54)

**Merged pull requests:**

- Restrict what characters tagged value prefixes \(tags\) are allowed to have [\#58](https://github.com/Kyorai/cuttlefish/pull/58) ([michaelklishin](https://github.com/michaelklishin))
- Parse tagged value tag up to the first colon \#56 [\#57](https://github.com/Kyorai/cuttlefish/pull/57) ([michaelklishin](https://github.com/michaelklishin))

## [v3.4.0](https://github.com/Kyorai/cuttlefish/tree/v3.4.0) (2024-08-06)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v3.3.0...v3.4.0)

**Merged pull requests:**

- New data types: "plain" binary, tagged binary [\#52](https://github.com/Kyorai/cuttlefish/pull/52) ([michaelklishin](https://github.com/michaelklishin))

## [v3.3.0](https://github.com/Kyorai/cuttlefish/tree/v3.3.0) (2024-08-06)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v3.2.0...v3.3.0)

**Closed issues:**

- Proposal: Support for Secrets via Tagged or Term Values [\#40](https://github.com/Kyorai/cuttlefish/issues/40)
- Include statement lacks whitespace before path [\#38](https://github.com/Kyorai/cuttlefish/issues/38)
- \[Feature request\] Provide a way to do character/string escaping [\#37](https://github.com/Kyorai/cuttlefish/issues/37)

**Merged pull requests:**

- Bump getopt to 1.0.3 [\#50](https://github.com/Kyorai/cuttlefish/pull/50) ([michaelklishin](https://github.com/michaelklishin))
- Require Erlang/OTP 24.x [\#49](https://github.com/Kyorai/cuttlefish/pull/49) ([michaelklishin](https://github.com/michaelklishin))
- Support for 'escaped values' [\#48](https://github.com/Kyorai/cuttlefish/pull/48) ([michaelklishin](https://github.com/michaelklishin))
- Grammar: require a space after the "include" directive [\#47](https://github.com/Kyorai/cuttlefish/pull/47) ([michaelklishin](https://github.com/michaelklishin))
- New data type: tagged string [\#46](https://github.com/Kyorai/cuttlefish/pull/46) ([michaelklishin](https://github.com/michaelklishin))
- Don't fail with an 'undefined' log level [\#45](https://github.com/Kyorai/cuttlefish/pull/45) ([michaelklishin](https://github.com/michaelklishin))
- Actions: use Rebar 3.23, drop testing on Erlang 23 [\#43](https://github.com/Kyorai/cuttlefish/pull/43) ([michaelklishin](https://github.com/michaelklishin))
- Re-arrange some tests [\#41](https://github.com/Kyorai/cuttlefish/pull/41) ([michaelklishin](https://github.com/michaelklishin))

## [v3.2.0](https://github.com/Kyorai/cuttlefish/tree/v3.2.0) (2023-02-13)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v3.1.0...v3.2.0)

**Merged pull requests:**

- GHA updates [\#34](https://github.com/Kyorai/cuttlefish/pull/34) ([lukebakken](https://github.com/lukebakken))
- Add new domain\_socket data type [\#33](https://github.com/Kyorai/cuttlefish/pull/33) ([codeadict](https://github.com/codeadict))

## [v3.1.0](https://github.com/Kyorai/cuttlefish/tree/v3.1.0) (2022-10-06)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v3.0.1...v3.1.0)

**Merged pull requests:**

- Support unicode throughout [\#31](https://github.com/Kyorai/cuttlefish/pull/31) ([lukebakken](https://github.com/lukebakken))
- Update GitHub actions [\#30](https://github.com/Kyorai/cuttlefish/pull/30) ([lukebakken](https://github.com/lukebakken))
- Fix typos [\#27](https://github.com/Kyorai/cuttlefish/pull/27) ([kianmeng](https://github.com/kianmeng))
- Update GH actions [\#26](https://github.com/Kyorai/cuttlefish/pull/26) ([lukebakken](https://github.com/lukebakken))

## [v3.0.1](https://github.com/Kyorai/cuttlefish/tree/v3.0.1) (2021-03-26)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v3.0.0...v3.0.1)

**Closed issues:**

- Config for unix socket listeners [\#23](https://github.com/Kyorai/cuttlefish/issues/23)
- List datatype [\#21](https://github.com/Kyorai/cuttlefish/issues/21)

**Merged pull requests:**

- Fix regex to include file [\#25](https://github.com/Kyorai/cuttlefish/pull/25) ([ansd](https://github.com/ansd))
- new datatype 'fqdn' [\#24](https://github.com/Kyorai/cuttlefish/pull/24) ([hmmr](https://github.com/hmmr))
- Add new list datatype [\#22](https://github.com/Kyorai/cuttlefish/pull/22) ([lrascao](https://github.com/lrascao))

## [v3.0.0](https://github.com/Kyorai/cuttlefish/tree/v3.0.0) (2021-03-13)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.7.0...v3.0.0)

**Closed issues:**

- Switch to built-in Erlang logger [\#19](https://github.com/Kyorai/cuttlefish/issues/19)

**Merged pull requests:**

- Replace Lager with OTP logger [\#20](https://github.com/Kyorai/cuttlefish/pull/20) ([michaelklishin](https://github.com/michaelklishin))
- Erlang 24 compatibility [\#18](https://github.com/Kyorai/cuttlefish/pull/18) ([michaelklishin](https://github.com/michaelklishin))

## [v2.7.0](https://github.com/Kyorai/cuttlefish/tree/v2.7.0) (2021-03-07)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.6.0...v2.7.0)

## [v2.6.0](https://github.com/Kyorai/cuttlefish/tree/v2.6.0) (2021-01-25)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.5.0...v2.6.0)

**Closed issues:**

- Read conf variables from the environment [\#16](https://github.com/Kyorai/cuttlefish/issues/16)

**Merged pull requests:**

- Allow environment variable config substitutions [\#17](https://github.com/Kyorai/cuttlefish/pull/17) ([lrascao](https://github.com/lrascao))

## [v2.5.0](https://github.com/Kyorai/cuttlefish/tree/v2.5.0) (2020-12-04)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.4.1...v2.5.0)

**Closed issues:**

- Update getopt to 1.0.2 when published [\#11](https://github.com/Kyorai/cuttlefish/issues/11)
- Environment vars [\#7](https://github.com/Kyorai/cuttlefish/issues/7)

**Merged pull requests:**

- Bump getopt to 1.0.2 [\#15](https://github.com/Kyorai/cuttlefish/pull/15) ([lrascao](https://github.com/lrascao))
- Allow shell expanded .conf values [\#14](https://github.com/Kyorai/cuttlefish/pull/14) ([lrascao](https://github.com/lrascao))
- Allow inclusion of other .conf files [\#13](https://github.com/Kyorai/cuttlefish/pull/13) ([lrascao](https://github.com/lrascao))
- Feature/standalone vm args generation [\#12](https://github.com/Kyorai/cuttlefish/pull/12) ([lrascao](https://github.com/lrascao))
- Add Windows and GitHub action status [\#10](https://github.com/Kyorai/cuttlefish/pull/10) ([lukebakken](https://github.com/lukebakken))
- Enable Github CI check on pull requests to master [\#9](https://github.com/Kyorai/cuttlefish/pull/9) ([lrascao](https://github.com/lrascao))
- Feature/standalone config generation [\#8](https://github.com/Kyorai/cuttlefish/pull/8) ([lrascao](https://github.com/lrascao))

## [v2.4.1](https://github.com/Kyorai/cuttlefish/tree/v2.4.1) (2020-08-20)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.4.0...v2.4.1)

## [v2.4.0](https://github.com/Kyorai/cuttlefish/tree/v2.4.0) (2020-07-22)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.3.0...v2.4.0)

**Merged pull requests:**

- Upgrade to lager 3.8.0 [\#6](https://github.com/Kyorai/cuttlefish/pull/6) ([lukebakken](https://github.com/lukebakken))

## [v2.3.0](https://github.com/Kyorai/cuttlefish/tree/v2.3.0) (2019-08-07)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.2.1...v2.3.0)

**Merged pull requests:**

- Repurpose -a to mean --advanced\_conf\_file as --app\_config is unused [\#5](https://github.com/Kyorai/cuttlefish/pull/5) ([michaelklishin](https://github.com/michaelklishin))

## [v2.2.1](https://github.com/Kyorai/cuttlefish/tree/v2.2.1) (2019-08-07)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.2.0...v2.2.1)

**Fixed bugs:**

- Ignore whitespace-only lines [\#2](https://github.com/Kyorai/cuttlefish/pull/2) ([lukebakken](https://github.com/lukebakken))

**Closed issues:**

- Cuttlefish chokes on lines with only whitespace characters in the config file [\#1](https://github.com/Kyorai/cuttlefish/issues/1)

**Merged pull requests:**

- Add appvoyer badge [\#4](https://github.com/Kyorai/cuttlefish/pull/4) ([Licenser](https://github.com/Licenser))
- Enable build on Windows [\#3](https://github.com/Kyorai/cuttlefish/pull/3) ([lukebakken](https://github.com/lukebakken))

## [v2.2.0](https://github.com/Kyorai/cuttlefish/tree/v2.2.0) (2019-07-12)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.1.4...v2.2.0)

## [v2.1.4](https://github.com/Kyorai/cuttlefish/tree/v2.1.4) (2018-07-08)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.1.3...v2.1.4)

## [v2.1.3](https://github.com/Kyorai/cuttlefish/tree/v2.1.3) (2018-05-31)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.1.2...v2.1.3)

## [v2.1.2](https://github.com/Kyorai/cuttlefish/tree/v2.1.2) (2018-05-31)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.1.1...v2.1.2)

## [v2.1.1](https://github.com/Kyorai/cuttlefish/tree/v2.1.1) (2018-05-23)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/v2.1.0...v2.1.1)

## [v2.1.0](https://github.com/Kyorai/cuttlefish/tree/v2.1.0) (2018-05-21)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.11...v2.1.0)

## [2.0.11](https://github.com/Kyorai/cuttlefish/tree/2.0.11) (2017-02-06)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.10...2.0.11)

## [2.0.10](https://github.com/Kyorai/cuttlefish/tree/2.0.10) (2016-10-12)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.9...2.0.10)

## [2.0.9](https://github.com/Kyorai/cuttlefish/tree/2.0.9) (2016-09-29)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.8...2.0.9)

## [2.0.8](https://github.com/Kyorai/cuttlefish/tree/2.0.8) (2016-09-22)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.7...2.0.8)

## [2.0.7](https://github.com/Kyorai/cuttlefish/tree/2.0.7) (2016-07-06)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.2p2...2.0.7)

## [2.0.2p2](https://github.com/Kyorai/cuttlefish/tree/2.0.2p2) (2016-03-03)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.6...2.0.2p2)

## [2.0.6](https://github.com/Kyorai/cuttlefish/tree/2.0.6) (2016-02-03)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.2p1...2.0.6)

## [2.0.2p1](https://github.com/Kyorai/cuttlefish/tree/2.0.2p1) (2016-01-29)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.5...2.0.2p1)

## [2.0.5](https://github.com/Kyorai/cuttlefish/tree/2.0.5) (2015-08-04)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.4...2.0.5)

## [2.0.4](https://github.com/Kyorai/cuttlefish/tree/2.0.4) (2015-07-21)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.3...2.0.4)

## [2.0.3](https://github.com/Kyorai/cuttlefish/tree/2.0.3) (2015-06-27)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.2...2.0.3)

## [2.0.2](https://github.com/Kyorai/cuttlefish/tree/2.0.2) (2015-06-10)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.0rc2...2.0.2)

## [2.0.0rc2](https://github.com/Kyorai/cuttlefish/tree/2.0.0rc2) (2014-08-13)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.0...2.0.0rc2)

## [2.0.0](https://github.com/Kyorai/cuttlefish/tree/2.0.0) (2014-08-13)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/2.0.0rc1...2.0.0)

## [2.0.0rc1](https://github.com/Kyorai/cuttlefish/tree/2.0.0rc1) (2014-05-20)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/0.1.0...2.0.0rc1)

## [0.1.0](https://github.com/Kyorai/cuttlefish/tree/0.1.0) (2013-10-16)

[Full Changelog](https://github.com/Kyorai/cuttlefish/compare/b43aa2d590727f6e7a0e2a330758161b296760f3...0.1.0)
