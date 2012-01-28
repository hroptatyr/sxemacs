#include <Xm/Xm.h>
#include <Xm/RowColumn.h>
#include <Xm/Scale.h>
#include <Xm/PushB.h>
#include <Xm/Label.h>
#include <Xm/Text.h>
#include <Xm/PanedW.h>
#include "ExternalClient.h"

XtAppContext xt_app_con;

void ScaleValueChangedCB(Widget scale, XtPointer app_data,
			 XtPointer widget_data)
{
	XmScaleCallbackStruct *xms = (XmScaleCallbackStruct *) widget_data;
	Widget label = (Widget) app_data;
	char labelarr[10];
	XmString labelstr;
#if 0
	int sz = snprintf(labelarr, sizeof(labelarr), "%d", xms->value);
	assert(sz>=0 && sz<sizeof(labelarr));
	labelstr = XmStringCreateLocalized(labelarr);
	XtVaSetValues(label, XmNlabelString, labelstr, NULL);
	XmStringFree(labelstr);
#endif
}

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
		no_ews = atoi(argv[1]);

	shell = XtAppInitialize(&xt_app_con, "Testmotif", NULL, 0,
				&argc, argv, NULL, NULL, 0);

	rowcolumn = XmCreateRowColumn(shell, "rowcolumn", NULL, 0);
	XtManageChild(rowcolumn);

	n = 0;
	XtSetArg(args[n], XmNtraversalOn, TRUE);
	n++;
#if 0
	label1 = XmCreateLabel(rowcolumn, "label1", args, n);
#endif
	label1 = XtVaCreateWidget("label1", xmLabelWidgetClass, rowcolumn,
				  XmNwidth, 50, XmNheight, 30,
				  XmNtraversalOn, TRUE, NULL);
	label2 = XmCreateLabel(rowcolumn, "label2", NULL, 0);
	scale = XmCreateScale(rowcolumn, "scale", NULL, 0);
	XtAddCallback(scale, XmNvalueChangedCallback, ScaleValueChangedCB,
		      label1);
	paned = XmCreatePanedWindow(rowcolumn, "paned", NULL, 0);
	n = 0;
	widlist[n++] = label1;
	widlist[n++] = label2;
	widlist[n++] = scale;
	widlist[n++] = paned;
	XtManageChildren(widlist, n);

	pushbutton = XmCreatePushButton(paned, "pushbutton", NULL, 0);
	text = XmCreateText(paned, "text", NULL, 0);
	for (i = 0; i < no_ews; i++) {
		int sz = snprintf(buf, sizeof(buf), "extcli%d", i);
		assert(sz>=0 && sz < sizeof(buf));
		emacscli[i] =
		    XtVaCreateWidget(buf, externalClientWidgetClass, paned,
				     XmNwidth, 500, XmNheight, 200,
				     XmNtraversalOn, TRUE,
				     NULL);
	}
	text2 = XmCreateText(paned, "text2", NULL, 0);
	n = 0;
	widlist[n++] = pushbutton;
	widlist[n++] = text;
	for (i = 0; i < no_ews; i++)
		widlist[n++] = emacscli[i];
	widlist[n++] = text2;
	XtManageChildren(widlist, n);

	XtRealizeWidget(shell);

	{
		XmString lab;
		char labarr[1000];
		char tmpbuf[50];

		strcpy(labarr, "window:");
		for (i = 0; i < no_ews; i++) {
			int sz = snprintf(tmpbuf, sizeof(tmpbuf),
					  " %d", XtWindow(emacscli[i]));
			assert(sz>=0 && sz<sizeof(tmpbuf));
			strcat(labarr, tmpbuf);
		}
		lab = XmStringCreateLocalized(labarr);
		XtVaSetValues(label2, XmNlabelString, lab, NULL);
		XmStringFree(lab);
	}

	XtAppMainLoop(xt_app_con);
}
