//---------------------------------------------------------------------------
#include <vcl.h>
#pragma hdrstop
USERES("AsgDemo.res");
USEFORM("Uasgdemo.cpp", Form1);
USEFORM("AsgAbout.cpp", About);
USEFORM("AsgFind.cpp", Find);
//---------------------------------------------------------------------------
WINAPI WinMain(HINSTANCE, HINSTANCE, LPSTR, int)
{
        try
        {
                 Application->Initialize();
                 Application->CreateForm(__classid(TForm1), &Form1);
                 Application->Run();
        }
        catch (Exception &exception)
        {
                 Application->ShowException(&exception);
        }
        return 0;
}
//---------------------------------------------------------------------------
