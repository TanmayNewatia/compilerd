'use client'

import React, { useEffect, useState } from "react";
import CodeEditorWindow from "../components/CodeEditorWindow";
import axios from "axios";
import { classnames } from "../utils/general";
import { languageOptions } from "../constants/languageOptions";
import toast, { Toaster } from 'react-hot-toast';
import { defineMonacoTheme } from "../lib/defineMonacoTheme";
import useKeyPress from "../hooks/useKeyPress";
import Footer from "../components/Footer";
import OutputWindow from "../components/OutputWindow";
import CustomInput from "../components/CustomInput";
import OutputDetails from "../components/OutputDetails";
import ThemeDropdown from "../components/dropdown/ThemeDropdown";
import LanguagesDropdown from "../components/dropdown/LanguagesDropdown";
import { GrPowerReset } from "react-icons/gr";

const Home = () => {
  const [code, setCode] = useState(languageOptions[0].defaultCode);
  const [customInput, setCustomInput] = useState("");
  const [outputDetails, setOutputDetails] = useState(null);
  const [processing, setProcessing] = useState(null);
  const [theme, setTheme] = useState("cobalt");
  const [language, setLanguage] = useState(languageOptions[0]);
  const [visibilityCustomInput, setVisibilityCustomInput] = useState(false);
  const enterPress = useKeyPress("Enter");
  const ctrlPress = useKeyPress("Control");

  const onSelectChange = (sl) => {
    setLanguage(sl);
    onChange("code", sl.defaultCode)
  };

  useEffect(() => {
    if (enterPress && ctrlPress) {
      toast.success('Code Submitted!');
      handleCompile();
    }
  }, [ctrlPress, enterPress]);

  const onChange = (action, data) => {
    switch (action) {
      case "code": {
        setCode(data);
        break;
      }
      default: {
        console.warn("Case not handled!", action, data);
      }
    }
  };

  const handleCompile = () => {
    setProcessing(true);
    const formData = {
      language_id: language.value,
      // encode source code in base64
      source_code: code,
      stdin: customInput,
    };
    const options = {
      method: "POST",
      url: process.env.NEXT_PUBLIC_URL || "http://localhost:4000/api/execute",
      params: { base64_encoded: "true", fields: "*" },
      headers: {
        "content-type": "application/json",
        "Content-Type": "application/json"
      },
      data: formData,
    };

    axios
      .request(options)
      .then(function (response) {
        console.log("res.data", response.data);
        setOutputDetails(response.data.output);
        showSuccessToast(`Compiled Successfully!`);
        console.log("response.data", response.data);
      })
      .catch((err) => {
        let error = err.response ? err.response.data : err;
        // get error status
        let status = err.response.status;
        console.log("status", status);
        setProcessing(false);
        console.log("catch block...", error);
      });
  };

  function handleThemeChange(th) {
    const theme = th;
    if (["light", "vs-dark"].includes(theme.value)) {
      setTheme(theme);
    } else {
      defineMonacoTheme(theme.value).then((_) => setTheme(theme));
    }
  }
  useEffect(() => {
    defineMonacoTheme("oceanic-next").then((_) =>
      setTheme({ value: "oceanic-next", label: "Oceanic Next" })
    );
  }, []);

  const showSuccessToast = (msg) => {
    toast.success(msg || `Compiled Successfully!`);
  };
  const showErrorToast = (msg, timer) => {
    toast.error(msg || `Something went wrong! Please try again.`);
  };

  return (
    <>
      <Toaster />
      <header className="flex items-center justify-between w-full h-[80px] overflow-x-hidden backdrop-blur-xl fixed top-0 z-[100] px-4">
        <div className="flex flex-col">
          <h1 className=" text-left hollow_effect text-4xl text-black">CompilerD</h1>
          <h2 className=" text-left text-xl text-white">Your Online Judge Platform</h2>
        </div>
        <div>
          <h2 className="text-right text-xl text-white">By <br /> Kalvium Community</h2>
        </div>
      </header>
      <div className="flex flex-row justify-end gap-4 mr-8 items-center pt-[80px]">
        <div className="px-4 py-2">
          <ThemeDropdown handleThemeChange={handleThemeChange} theme={theme} />
        </div>
        <div className="px-4 py-2">
          <LanguagesDropdown onSelectChange={onSelectChange} />
        </div>
        <div className="px-2 py-2 bg-white text-black rounded border-1 border-black shadow-[5px_5px_0_0_rgb(255,255,255)] hover:translate-x-[5px] hover:translate-y-[5px] hover:shadow-none transition duration-200 cursor-pointer" onClick={() => { setCode(language.defaultCode); toast.success("Code Reset Completed!") }}>
          <GrPowerReset />
        </div>
      </div>
      <div className="flex flex-row space-x-4 items-start px-4 py-4 max-md:flex-col-reverse mb-8 gap-4">
        <div className="right-container flex flex-shrink-0 w-[30%] flex-col max-md:w-[100%]">
          <OutputWindow outputDetails={outputDetails} />
          <div className="flex flex-col items-end">
            {(visibilityCustomInput) && <CustomInput
              customInput={customInput}
              setCustomInput={setCustomInput}
            />}
            <div className="flex items-end gap-8">
              <label className="cursor-pointer container flex gap-2 items-center">
                <input type="checkbox" className="hidden" />
                <svg viewBox="0 0 64 64" height="1em" width="1em" className="overflow-visible" onClick={() => { setVisibilityCustomInput(!visibilityCustomInput) }}>
                  <path d="M 0 16 V 56 A 8 8 90 0 0 8 64 H 56 A 8 8 90 0 0 64 56 V 8 A 8 8 90 0 0 56 0 H 8 A 8 8 90 0 0 0 8 V 16 L 32 48 L 64 16 V 8 A 8 8 90 0 0 56 0 H 8 A 8 8 90 0 0 0 8 V 56 A 8 8 90 0 0 8 64 H 56 A 8 8 90 0 0 64 56 V 16" pathLength="575.0541381835938" className="path"></path>
                </svg>
                <p onClick={() => { setVisibilityCustomInput(!visibilityCustomInput) }}>Custom Input</p>
              </label>
              <button
                onClick={handleCompile}
                disabled={!code}
                className={classnames(
                  "mt-4 border-2 border-black z-10 rounded-md shadow-[5px_5px_0px_0px_rgba(255,255,255)] px-4 py-2 hover:translate-x-[5px] hover:translate-y-[5px] hover:shadow-none transition duration-200 bg-white text-black flex-shrink-0",
                  !code ? "opacity-50" : ""
                )}
              >
                {processing ? "Processing..." : "Compile and Execute"}
              </button>
            </div>
          </div>
          {outputDetails && <OutputDetails outputDetails={outputDetails} />}
        </div>
        <div className="flex flex-col w-full h-full justify-start items-end">
          <CodeEditorWindow
            code={code}
            onChange={onChange}
            language={language?.value}
            theme={theme?.value}

          />
        </div>
      </div>
      <Footer />
    </>
  );
};
export default Home;