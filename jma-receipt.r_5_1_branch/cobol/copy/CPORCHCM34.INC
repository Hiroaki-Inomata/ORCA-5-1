      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
      * Copyright(C) 2002 JMA (Japan Medical Association)
      *
      * This program is part of "JMA standard receipt software".
      *
      *     This program is distributed in the hope that it will be useful
      * for further advancement in medical care, according to JMA Open
      * Source License, but WITHOUT ANY WARRANTY.
      *     Everyone is granted permission to use, copy, modify and
      * redistribute this program, but only under the conditions described
      * in the JMA Open Source License. You should have received a copy of
      * this license along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *----------------------------------------------------------*
      *    ��ץ����ɥ��֥ѥ�᥿��  COPY (CPORCHCM34.INC)
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  ORCHCM34AREA.
           03  ORCHCM34-IN.
      *        ������ʬ
      *       ��������饤�󡿣������
               05  ORCHCM34-KBN            PIC X(01).
      *        ����ǯ��
               05  ORCHCM34-SRYYM          PIC X(06).
      *        ������ʬ
               05  ORCHCM34-NYUGAIKBN      PIC X(01).
      *        ��̾ɽ����ɽ����ʬ
               05  ORCHCM34-HIDEBYOKBN     PIC X(01).
      *        ���Ųʶ�ʬ
               05  ORCHCM34-SRYKAKBN       PIC X(01).
      *        ���Ų�
               05  ORCHCM34-SRYKA          PIC X(02).
      *        ���Ų�����
               05  ORCHCM34-SRYKADAY-G.
                   07  ORCHCM34-SRYKADAY   PIC 9(01)
                                           OCCURS  31.
      *        �ݸ���ʬ 0:�ݸ���˲���1:����2:����3:����¾
               05  ORCHCM34-HOKEN          PIC X(01).
      *        ������ʬ 1:�оݿ��Ź԰٤��ʤ��Ƥ��������
               05  ORCHCM34-PRTCTRLKBN     PIC X(01).
      *        ���ֶ�ʬ 1:���ֻ��ꤢ��
               05  ORCHCM34-KIKANKBN       PIC X(01).
               05  ORCHCM34-STDAY          PIC 9(02).
               05  ORCHCM34-EDDAY          PIC 9(02).
               05  ORCHCM34-STSRYYMD       PIC X(08).
               05  ORCHCM34-EDSRYYMD       PIC X(08).
      *        �����ɽ����ʬ
               05  ORCHCM34-HIDEHOKATSU    PIC X(01).
      *        ���Ŷ�ʬ�����ģ������Խ���ʬ
               05  ORCHCM34-EDIT-MEDADL    PIC X(01).
      *        ���ѥ�᥿
               05  ORCHCM34-PRTAREA        PIC X(25000).
      *
           03  ORCHCM34-OT.
      *        �Խ��ڡ�����
               05  ORCHCM34-PAGE           PIC 9(08).
      *        �꥿���󥳡��ɡʣ���������ʳ����۾��
               05  ORCHCM34-RC             PIC 9(02).