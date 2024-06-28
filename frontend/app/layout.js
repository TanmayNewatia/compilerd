import { Barlow_Condensed } from "next/font/google";
import "./globals.css";
import { NextProvider } from "@/components/provider/NextProvider"

const barlow = Barlow_Condensed({ subsets: ["latin"], weight: ["100", "200", "300", "400", "500", "600", "700", "800", "900"] });

export const metadata = {
  title: "CompilerD",
  description: "Your Online Code Judge",
};

export default function RootLayout({ children }) {
  return (
    <html lang="en">
      <body className={barlow.className}>
        <NextProvider>
          {children}
        </NextProvider>
      </body>
    </html>
  );
}
