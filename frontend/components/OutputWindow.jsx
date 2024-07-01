import React from "react";

const OutputWindow = ({ outputDetails }) => {
    const getOutput = () => {
        <pre className="px-2 py-1 font-normal text-xs text-green-500">
            {(outputDetails.val) !== null
                ? `${(outputDetails.val)}`
                : null}
        </pre>
    };
    return (
        <>
            <h1 className="font-bold text-xl text-white mb-2">
                Output
            </h1>
            <div className="w-full h-56 bg-[#1e293b] rounded-md text-white font-normal text-sm overflow-y-auto p-2">
                {outputDetails ? <>{getOutput()}</> : "Compile the code to check the output!"}
            </div>
        </>
    );
};

export default OutputWindow;