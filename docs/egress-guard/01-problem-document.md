# Problem Document: Consumer Data-Egress Defence on Android

**Working title:** Egress Guard (placeholder)
**Status:** Draft v0.1, for discussion. Specs come after this is agreed.
**Date:** 2026-11-01
**Author:** Prakhar Kumar (with research support from Claude)

---

## 1. Problem statement

Companies stop sensitive data from leaving through layered controls: DLP, MDM, network monitoring, a SOC watching live alerts, and policy enforcement. Consumers have none of that. Their phone holds the material that gets used against them: private photos, the contact list, KYC selfies, ID documents, screenshots of OTPs and recovery phrases. The main way it leaks is not an exploit. **The user is persuaded to install an app and grant it a permission.**

Predatory loan apps, fake "KYC update" apps, OCR stealers and sextortion operators turn that access into leverage: harassing the victim's contacts, sending morphed intimate images, and extorting money. Android's security architecture is now very strong (sandboxing, scoped storage, the Photo Picker, profiles, Private Space, Play Protect), but:

1. Users don't know which of these protections they have, which they've switched off, or how to arrange their phone to benefit from them.
2. The OS asks for consent at the moment of greatest pressure and least understanding (the user needs a loan now, or a stranger on a call says to tap "Allow").
3. Once access is granted, nothing on the consumer side watches what happens next, and nothing helps the victim when extortion starts.

**The opportunity:** a consumer app that acts like a personal security team, built **on top of** Android's controls rather than around them. It organises the device into trust zones, checks risky grants in context, gives visibility, and supports the user after an incident. It cannot be a "super-antivirus" with privileges Android doesn't give third-party apps, and it shouldn't try to be one.

---

## 2. The user's own scenario (worked example)

> Two profiles: *Work* (WhatsApp, Teams, important apps) and *Personal* (most new downloads). Worry: sensitive images received on WhatsApp may be readable by any app with media access.

What Android actually does in this setup:

| Question | Answer | Confidence |
|---|---|---|
| Can a **personal-profile** app with full media permission read WhatsApp images received in the **work** profile? | **No, by default.** Each profile is a separate Android user with its own storage and its own MediaStore. Apps, data and accounts in one profile cannot reach the other's. | High ([Bayton, cross-profile data sharing](https://bayton.org/android/android-enterprise-faq/cross-profile-data-sharing/)) |
| Can another **work-profile** app with media permission read them? | **Yes.** With WhatsApp's "Media visibility" on, received media is indexed into that profile's shared media store, and any app in the same profile holding `READ_MEDIA_IMAGES` can read it. "Media visibility" only controls display in the gallery. It is not a privacy control. | High for the mechanism; verify the exact `Android/media/com.whatsapp/…` indexing behaviour on the target device |
| Are there legitimate cross-profile paths? | Yes: share sheet, the Photo Picker's work/personal tabs (when policy allows), copy/paste, and cloud sync (Drive/Photos/OneDrive). Each is a possible leak if the "personal" side is untrusted. | High ([Microsoft: media in Android work profile](https://support.microsoft.com/en-US/Office/access-images-and-other-media-in-your-android-work-profile)) |
| Who controls the work profile? | If an employer's MDM created it, the employer is the profile owner. They can apply policy and wipe it, and usually can't read personal files, but **sensitive personal images then sit in an employer-controlled container.** If it was created with a consumer tool (Shelter/Island-style), the user is in control. | High |

**Takeaway:** the user's instinct is mostly right, and the setup is already better than most people's. The remaining risks are (a) anything installed into the *same* profile as WhatsApp, (b) cross-profile sharing and cloud backups, (c) the trust inversion: the "safe" profile holding intimate personal media may be employer-managed, and (d) KYC selfies and contacts given to risky apps on the *personal* side, which is enough for extortion even without a single gallery photo (see §4.3).

This pattern ("keep trusted apps and sensitive data in one zone, quarantine untrusted apps in another") is the core idea. Today only technically skilled users do it. The product should make it the default for everyone.

---

## 3. Threat landscape (evidence)

### 3.1 Predatory loan apps / SpyLoan
- **ESET (Dec 2023):** 18 SpyLoan apps on Google Play, **12M+ downloads**. They harvested contacts, SMS, photos and location, then used them for harassment and extortion. They passed review with compliant privacy policies and "transparent" permission prompts. ([ESET WeLiveSecurity](https://www.welivesecurity.com/en/eset-research/beware-predatory-fintech-loan-sharks-use-android-apps-reach-new-depths/), [BleepingComputer](https://www.bleepingcomputer.com/news/security/spyloan-android-malware-on-google-play-downloaded-12-million-times/))
- **McAfee (Dec 2024):** 15 more SpyLoan apps, **8M+ installs**. SpyLoan activity **rose 75%+ from Q2 to Q3 2024.** Some apps were simply *updated* to reduce the bad behaviour instead of being removed. ([The Hacker News](https://thehackernews.com/2024/12/8-million-android-users-hit-by-spyloan.html))
- **India case pattern:** loans as small as ₹2,000–₹10,000 lead to morphed nude images sent to the victim's contacts. ([Deccan Herald](https://www.deccanherald.com/india/karnataka/bengaluru/woman-faces-threats-with-morphed-images-over-rs-2000-loan-in-bengaluru-3803954), [Hindustan Times via PressReader](https://www.pressreader.com/india/hindustan-times-st-mumbai/20240410/281784224122611))
- **Scale in India:** an I4C breakdown of 2023 complaint modus operandi reportedly lists **sextortion ~24%** and **illegal loan apps ~23%** of categorised cases. ([NCERT/CIET I4C slides](https://ciet.ncert.gov.in/storage/app/public/files/17/Workshop_and_training_files/fsce/day1_ppt.pdf); the slide is garbled, so verify against the I4C original.) I4C reported ~**7,000 cybercrime complaints per day** in May 2024. ([CIET](https://ciet.ncert.gov.in/storage/app/public/files/17/webinar01/August2024/12082024.pdf))

### 3.2 Gallery-scraping / OCR stealers
- **SparkCat (Kaspersky, Feb 2025):** the first OCR stealer found in *both* App Store and Google Play. It asked for gallery access, ran on-device OCR (Google ML Kit) over every image, and uploaded the ones that matched keywords. **242k+ Play downloads**, hidden in messengers, AI assistants and food delivery apps. It targeted crypto recovery phrases but "could easily be repurposed." ([Kaspersky](https://www.kaspersky.com/blog/ios-android-ocr-stealer-sparkcat/52980/))
- Lesson: **a single "Allow all photos" grant is a full egress channel**, and on-device ML makes finding the valuable images cheap.

### 3.3 Sextortion (global)
- **NCMEC:** **50,000+ financial-sextortion reports in 2025** (~137/day), up from 36,000+ in 2024. Victims are mostly boys aged 14–17. ([NCMEC](https://us.missingkids.org/blog/2026/ncmec-releases-new-sextortion-data-2025))
- Much sextortion starts on social or messaging apps, not with on-device theft. It is still in scope for the **response** part of the product (§6, P6).

### 3.4 Social-engineering-driven device takeover
- Banking trojans (Anatsa, Crocodilus, TrickMo) abuse **Accessibility** to read the screen and act on it. Droppers have bypassed Android 13's *Restricted Settings*. Anatsa reached ~90k downloads on Play disguised as a PDF viewer (Jul 2025). Google added **tighter limits on the Accessibility API in March 2026.** ([Help Net Security](https://www.helpnetsecurity.com/2026/03/19/google-android-accessibility-api-restrictions/), [Malwarebytes](https://www.malwarebytes.com/blog/mobile/2026/03/google-cracks-down-on-android-apps-abusing-accessibility), [SC World](https://www.scworld.com/news/android-trojan-crocodilus-hijacks-accessibility-settings-for-control))
- Common scam script: a caller ("bank/KYC/courier/police") talks the victim through sideloading an APK and granting Accessibility or screen sharing.

### 3.5 Attack chain (common shape)
```
Lure (urgent need / fear / curiosity)
  → Install (Play, or sideloaded APK sent via WhatsApp/Telegram/SMS)
    → Consent capture (media "Allow all", contacts, SMS, notifications, accessibility, KYC selfie)
      → Harvest (bulk read; on-device OCR/ML triage)
        → Egress (upload to C2 / cloud, often in the background)
          → Weaponise (morph images; message contacts; threaten)
            → Extort (payment demands, escalating harassment)
```
Every arrow is a place to intervene. Android already covers some of them well. The product should cover the rest.

---

## 4. Android security architecture: what helps, and what's still open

### 4.1 Platform controls to build on

| Control | What it gives | Version / status | Gap for consumers |
|---|---|---|---|
| **App sandbox + scoped storage** | Apps can't read other apps' private files. `READ_EXTERNAL_STORAGE` has no effect from API 33. | Mature | Shared media is still one bucket per profile |
| **Granular media permissions** `READ_MEDIA_IMAGES/VIDEO` | Separates photos/videos from audio/files | Android 13+ | Still all-or-nothing within photos |
| **Selected Photos Access** `READ_MEDIA_VISUAL_USER_SELECTED` | User grants only chosen items | Android 14+ ([docs](https://developer.android.com/about/versions/14/changes/partial-photo-video-access)) | Users still tap "Allow all"; apps that got full access before upgrading keep it |
| **Photo Picker** | No permission needed. App gets only what the user picks. | Android 11+ via Play system updates; backported | Adoption depends on the app |
| **Play Photo & Video policy** | Broad media permissions only for apps whose *core* function needs them, with a declaration. One-off uses must use a system picker. | Enforced from **28 May 2025** ([Play policy](https://support.google.com/googleplay/android-developer/answer/14115180)) | Doesn't reach sideloaded apps. Enforcement is reactive. |
| **Contact Picker** `ACTION_PICK_CONTACTS` | Share only the selected contacts | Android 17 ([Android dev blog](https://developer.android.com/blog/posts/contact-picker-privacy-first-contact-sharing)) | Very new; older devices still use `READ_CONTACTS` |
| **Work profile** | Separate user, storage, MediaStore and accounts | Mature | Consumer setup is hard; usually employer-owned |
| **Private Space** | Isolated, lockable, hideable profile. Its apps vanish from the picker/share sheet while locked. | Android 15+ ([AOSP](https://source.android.com/docs/security/features/private-space)) | Most users don't know it exists. Only one per device. |
| **Permission auto-reset / hibernation** | Revokes grants from unused apps | Android 11+ ([docs](https://developer.android.com/topic/performance/app-hibernation)) | "A few months" is too long for a loan app used once |
| **Privacy Dashboard / indicators** | User sees recent permission use | Android 12+ | **Not readable by third-party apps** (see 4.2) |
| **Restricted Settings** | Blocks sideloaded apps from Accessibility/Notification access | Android 13+ | Bypassed by session-installer droppers |
| **Play Protect** (incl. live threat detection) | On-device malware scanning | Ongoing | Signature/behaviour based; cat-and-mouse |
| **Advanced Protection mode** | One switch: blocks sideloading, turns off 2G, Safe Browsing live threat protection, scam detection, MTE | Android 16+ ([Android Authority](https://www.androidauthority.com/android-16-advanced-protection-mode-security-features-apk-teardown-3545203/)) | Too strict for most users; little known |
| **Developer verification for sideloaded apps** | Certified devices block installs from unverified developers | Enforced **30 Sep 2026** in BR/ID/SG/TH; global in 2027. India not in the first wave. ([The Hacker News](https://thehackernews.com/2026/06/google-sets-sept-30-deadline-for.html)) | adb / "advanced flow" remains; doesn't apply in India yet |
| **Play personal-loan policy (India)** | Loan apps barred from photos, contacts, location and call logs (since May 2023). Must be on the government-approved DLA list (new apps from **30 Oct 2025**). | ([Play help](https://support.google.com/googleplay/android-developer/answer/9876821)) | Off-Play apps and apps posing as non-loan apps escape it |

### 4.2 What a third-party app **cannot** do (design constraints)
These limits decide what the product can be:

- **Can't see which app read which photo.** App-op history is readable only by system components. `OnOpNotedCallback` covers only the app's own accesses. ([AppOpsManager](https://developer.android.com/reference/android/app/AppOpsManager.OnOpNotedCallback))
- **Can't revoke another app's permissions**, unless it is the *profile owner/device owner* of that profile (see 4.3). Otherwise it can only deep-link the user to the right settings screen.
- **Can't inspect other apps' memory, files or encrypted traffic.**
- **Can't watch across profiles.** An app sees only its own profile.
- **Can't run indefinitely in the background.** Battery and background limits apply.
- **Play-gated sensitive capabilities:** `QUERY_ALL_PACKAGES` is allowed for antivirus/security with a declaration ([policy](https://support.google.com/googleplay/android-developer/answer/10158779)). `VpnService` is allowed for device security/firewall with a declaration and prominent disclosure ([policy](https://support.google.com/googleplay/android-developer/answer/12564964)). Accessibility use is increasingly restricted and should be **avoided**. Using malware techniques to fight malware hurts trust and gets apps removed.

### 4.3 What a third-party app **can** do (legitimate levers)

1. **Inventory and permission posture.** With an antivirus-category `QUERY_ALL_PACKAGES` declaration: list installed apps, their requested and **granted** permissions, installer source (Play vs sideloaded), first-install and last-update time, target SDK, and whether they hold Accessibility, Notification Listener, Device Admin, overlay or install-unknown-apps access.
2. **Usage signals.** With user-granted `PACKAGE_USAGE_STATS`: last-used time, so it can flag "used once 40 days ago and still holds photos + contacts."
3. **Become the owner of a quarantine profile.** A consumer app can create and **own a managed (work) profile**, as Shelter/Island do. As profile owner it can, *inside that profile*: set permission policy (auto-deny media/contacts/SMS), pre-grant or deny specific permissions per app, force an always-on VPN, restrict cross-profile sharing and copy/paste, freeze/suspend apps, and on Android 12+ enable network logging for the work profile on a personal device. **This is the strongest legitimate lever:** untrusted apps (loan apps, random utilities, "free" AI tools) go into a sandbox the app controls, where photos and contacts simply aren't there to steal.
   - Caveat: if the user's work profile is already employer-managed, Android allows only one work profile. Private Space (Android 15+) is the other isolation zone available.
4. **Local network visibility.** An on-device `VpnService` firewall (NetGuard/TrackerControl pattern, no remote server) can attribute traffic to apps: who is uploading large volumes in the background, to which domains, right after a media grant. Constraints: one VPN at a time per profile, it conflicts with a user's real VPN, and Play's disclosure rules apply.
5. **Pre-install and pre-grant checks.** Check an app against the RBI DLA directory or government approved-lender list, its developer identity, Play listing age and installs, and threat-intel feeds before the user installs or grants.
6. **Guided hardening.** Detect current settings and walk the user through fixing them: enable Private Space, set WhatsApp media visibility off per chat, turn off "Install unknown apps" for messengers and browsers, move to Advanced Protection where suitable, review Selected-Photos grants, remove stale Accessibility grants.
7. **Media hygiene inside its own vault.** An encrypted, app-private vault for sensitive images (scoped storage, invisible to MediaStore), with "move out of gallery" flows. On-device classification can point out which images are sensitive (intimate, ID documents, screenshots of secrets) so the user can move them. *This is the same capability SparkCat used, pointed the other way.*
8. **Incident response.** Playbooks, evidence capture, reporting to India's 1930 helpline / cybercrime.gov.in, StopNCII.org hashing (adults) / NCMEC Take It Down (minors), the MeitY NCII takedown SOP, and pre-written messages to warn contacts.

---

## 5. Users and needs

| Persona | Context | Primary need |
|---|---|---|
| **P1: Prepared professional** (this user) | Already uses profiles; tech-literate | Make sure the setup really holds; find gaps; keep it tidy over time |
| **P2: Credit-stressed borrower** | Urgent need, low trust in banks, installs instant-loan apps | Tell real lenders from predatory ones *before* granting anything; limit damage if already installed |
| **P3: Everyday user / family member** | Accepts every prompt; WhatsApp media goes to gallery | Simple defaults and plain-language warnings; one-tap "lock it down" |
| **P4: Teen / young adult** | Main target of financial sextortion | Don't-pay guidance, fast takedown, shame-free help |
| **P5: Victim in crisis** | Extortion in progress | Calm step-by-step response, evidence, reporting, contact warnings |

**Jobs to be done**
1. "Show me who can see my private photos and contacts right now, in plain language."
2. "Before I install or allow this, tell me whether it's safe."
3. "Set up my phone so untrusted apps physically can't reach my sensitive stuff."
4. "Warn me if something starts behaving like a data thief."
5. "If I'm being threatened, tell me exactly what to do now."

---

## 6. Problem areas (what the solution must address)

- **P1: No visibility.** Users can't see their exposure. Android shows it permission by permission, app by app, profile by profile, with no overall view.
- **P2: Consent under pressure.** The riskiest grants happen in a hurry and while being socially engineered.
- **P3: No zones.** Sensitive data and untrusted apps share one profile by default. Profiles and Private Space exist but are hard to set up and not used.
- **P4: Sensitive media is everywhere.** Messenger downloads, screenshots and KYC photos all end up in the shared gallery.
- **P5: No consumer egress monitoring.** Nothing notices the "granted media → bulk upload" pattern.
- **P6: No help after an incident.** Victims panic, pay, and don't know about 1930, StopNCII, the NCII SOP, or that warning contacts in advance takes away the leverage.
- **P7: Permissions go stale.** One-time apps keep powerful grants for months.

---

## 7. Goals and non-goals

**Goals**
- G1. Reduce what can leak: get sensitive media and contacts out of reach of untrusted apps by default.
- G2. Step in at the decision point (install/grant) with context-aware risk signals.
- G3. Give an understandable, profile-aware picture of the device's exposure.
- G4. Detect and alert on likely exfiltration behaviour using only allowed APIs.
- G5. Give a reliable, locally relevant (India first) incident-response companion.
- G6. Be trustworthy: on-device by default, minimal permissions, open about what it can and can't do.

**Non-goals**
- Not a replacement for Play Protect or a signature-based AV engine.
- No root, no exploits, no Accessibility-based control of other apps.
- Not an MDM for employers. It must coexist with an employer's work profile, not take it over.
- Not able to guarantee prevention. The honest framing is *less exposure, earlier warning, faster recovery*.
- No cloud upload of user media, ever (classification stays on device).

---

## 8. Constraints and risks

| # | Risk | Notes / mitigation direction |
|---|---|---|
| R1 | **Play policy rejection** for `QUERY_ALL_PACKAGES`, `VpnService`, `PACKAGE_USAGE_STATS`, managed-profile provisioning | Fit clearly into the "device security" category, with prominent disclosures and demo videos. Ship a capability tier that works without each sensitive permission. |
| R2 | **The app itself is a high-value target** (it knows exposure and holds a vault) | On-device only, strong crypto (Android Keystore / StrongBox), no media telemetry, open-source core to build trust |
| R3 | **One-work-profile limit**; conflict with employer MDM | Detect it. Fall back to Private Space plus permission-hygiene mode. |
| R4 | **VPN conflict** with the user's real VPN; battery cost | Make network monitoring optional and time-boxed (e.g. "watch new app for 72h") |
| R5 | **OEM fragmentation** (Samsung Secure Folder, Xiaomi Second Space, etc.) | Capability detection; OEM-specific guidance |
| R6 | **False positives** cause fatigue; **false negatives** cause false confidence | Calibrated messaging; never say "safe," say "no known risks found" |
| R7 | **Legal/regulatory**: DPDP Act and Rules 2025 obligations as a data fiduciary; sensitive-content handling | Minimise data; process locally; keep consent records ([DPDP Rules 2025 notified 14 Nov 2025](https://www.ey.com/en_in/insights/cybersecurity/transforming-data-privacy-digital-personal-data-protection-rules-2025)) |
| R8 | **Cat-and-mouse**: attackers adapt (e.g., morph using the WhatsApp DP or KYC selfie, needing no gallery access) | Zone-based design beats signature design; include contacts/selfie exposure and incident response, not just the gallery |

---

## 9. Success measures (draft)

- % of users whose sensitive media sits outside the profile(s) where untrusted apps live (target: majority within 7 days of onboarding)
- Median count of "untrusted app + broad photo/contacts grant" pairs per device: before vs after
- Stale-grant reduction: high-risk grants unused for more than 14 days
- Pre-grant check usage rate, and % of checks where the user backs out of a flagged grant
- Incident playbook completion and time-to-report (1930 / NCRP)
- Trust: uninstall rate, Play rating, independent audit outcome

---

## 10. Market and prior art (to research further during spec)

- **Platform:** Google Play Protect, Advanced Protection, Private Space; Samsung Secure Folder / Knox; Xiaomi Second Space.
- **Isolation tools:** Shelter, Island, Insular (consumer work-profile managers): technically capable, poor UX for non-experts.
- **Network:** NetGuard, TrackerControl, RethinkDNS: local VPN firewalls, aimed at power users.
- **AV suites:** McAfee, Norton, Kaspersky, Bitdefender: signature and scam detection, little about data-egress posture or zones.
- **Gallery vaults:** many "photo locker" apps, which are themselves often low-trust.
- **Response:** StopNCII.org, NCMEC Take It Down, cybercrime.gov.in / 1930, MeitY NCII SOP (Nov 2025) ([PIB/MeitY](https://www.digitalindia.gov.in/press_release/meity-issues-standard-operating-procedure-to-curtail-dissemination-of-non-consensual-intimate-imagery-ncii-content/)).
- **Gap:** no product combines **zoning + posture + pre-grant checks + egress monitoring + incident response** around the consumer data-extortion threat, built natively on Android's controls.

---

## 11. Open questions for Prakhar

1. **Who owns your work profile?** Created by your employer's MDM (Intune etc.) or by a consumer tool (Shelter/Island)? This changes whether we can use it, or whether the app should create a **third zone** via Private Space.
2. **Target market:** India-first (loan apps, 1930, RBI DLA directory, Hindi and regional languages) or global from day one?
3. **Distribution:** Play Store only (accepts policy limits), or also an F-Droid/direct build with more capability for power users?
4. **Minimum Android version:** 13 (granular media), 14 (selected photos), or 15 (Private Space)? This decides how much of the platform we can rely on.
5. **Open-source core?** Strongly recommended for a security app asking for sensitive permissions.
6. **Business model:** free/NGO-style, freemium, or B2B2C (banks/telcos offering it to customers)? This affects trust and data posture.
7. **Repo:** move to a dedicated repo once you create it (this doc currently lives in `gis_proj/docs/egress-guard/`).

---

## 12. Next step

Once this problem framing is agreed: write the **product spec**, covering capability tiers by permission grant, the zone model (Personal / Trusted / Quarantine / Vault), core flows (onboarding audit, pre-install check, quarantine install, media triage, egress alert, crisis mode), architecture (Kotlin, on-device ML, DevicePolicyManager profile-owner module, local VpnService module), and the Play policy compliance plan.
