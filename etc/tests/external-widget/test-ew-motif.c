#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/PanedW.h>
#include "ExternalClient.h"

#ifdef TOOLTALK
#include <desktop/tt_c.h>
char *HxProcID;
#endif

XtAppContext xt_app_con;

void ScaleValueChangedCB(Widget scale, XtPointer app_data, XtPointer widget_data)
{
    XmScaleCallbackStruct *xms = (XmScaleCallbackStruct *) widget_data;
    Widget label = (Widget) app_data;
    char labelarr[10];
    XmString labelstr;
#if 0
    sprintf(labelarr, "%d", xms->value);
    labelstr = XmStringCreateLocalized(labelarr);
    XtVaSetValues(label, XmNlabelString, labelstr, NULL);
    XmStringFree(labelstr);
#endif
}

#ifdef TOOLTALK
static void
handle_tt_input(XtPointer client_data, int *source, XtInputId *id)
{
  Tt_message m = tt_message_receive();

  if (m && !(tt_ptr_error(m))) {
    tt_message_destroy(m);
  }
}

Tt_status
HxInitializeToolTalk()
{
  static Boolean initialized = FALSE;

  if (!initialized) {
    int fd;
    Tt_status status;

    HxProcID = tt_open();
    fd = tt_fd();
    if (TT_OK != (status = tt_session_join( tt_default_session() )))
      return status;
    (void)XtAppAddInput(xt_app_con, fd, (void *)XtInputReadMask, handle_tt_input, NULL);
    initialized = TRUE;
  }

  return TT_OK;
}
#endif

main(int argc, char **argv)
{
    Widget shell, rowcolumn, scale, pushbutton, label1, label2, text;
    Widget paned, text2;
    int n, i;
    Widget widlist[100];
    Widget emacscli[100];
    Arg args[100];
    int no_ews = 1;
    char buf[100];

    if (argc > 1)
      no_ews = atoi (argv[1]);

    shell = XtAppInitialize(&xt_app_con, "Testmotif", NULL, 0,
	&argc, argv, NULL, NULL, 0);

#ifdef TOOLTALK
    HxInitializeToolTalk();
#endif

    rowcolumn = XmCreateRowColumn(shell, "rowcolumn", NULL, 0);
    XtManageChild(rowcolumn);

    n = 0;
    XtSetArg(args[n], XmNtraversalOn, TRUE); n++;
#if 0
    label1 = XmCreateLabel(rowcolumn, "label1", args, n);
#endif
    label1 = XtVaCreateWidget("label1", xmLabelWidgetClass, rowcolumn,
			      XmNwidth, 50, XmNheight, 30,
			      XmNtraversalOn, TRUE, NULL);
    label2 = XmCreateLabel(rowcolumn, "label2", NULL, 0);
    scale = XmCreateScale(rowcolumn, "scale", NULL, 0);
    XtAddCallback(scale, XmNvalueChangedCallback, ScaleValueChangedCB, label1);
    paned = XmCreatePanedWindow(rowcolumn, "paned", NULL, 0);
    n = 0;
    widlist[n++] = label1;
    widlist[n++] = label2;
    widlist[n++] = scale;
    widlist[n++] = paned;
    XtManageChildren(widlist, n);

    pushbutton = XmCreatePushButton(paned, "pushbutton", NULL, 0);
    text = XmCreateText(paned, "text", NULL, 0);
    for (i=0; i<no_ews; i++) {
      sprintf (buf, "extcli%d", i);
      emacscli[i] = XtVaCreateWidget(buf, externalClientWidgetClass, paned,
				     XmNwidth, 500, XmNheight, 200,
				     XmNtraversalOn, TRUE,
#ifdef TOOLTALK
				     XtNuseToolTalk, TRUE,
#endif
				     NULL);
    }
    text2 = XmCreateText(paned, "text2", NULL, 0);
    n = 0;
    widlist[n++] = pushbutton;
    widlist[n++] = text;
    for (i=0; i<no_ews; i++)
      widlist[n++] = emacscli[i];
    widlist[n++] = text2;
    XtManageChildren(widlist, n);

    XtRealizeWidget(shell);

    {
      XmString lab;
      char labarr[1000];
      char tmpbuf[50];
      
      strcpy (labarr, "window:");
      for (i=0; i<no_ews; i++) {
	sprintf (tmpbuf, " %d", XtWindow(emacscli[i]));
	strcat (labarr, tmpbuf);
      }
      lab = XmStringCreateLocalized(labarr);
      XtVaSetValues(label2, XmNlabelString, lab, NULL);
      XmStringFree(lab);
    }
    
    XtAppMainLoop(xt_app_con);
}
