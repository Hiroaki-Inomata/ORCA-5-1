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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *  05.00.01    NACL-ƣ��    18/05/01  �����ե饰���ɲ�
      *                                     ����̾�����ɤ򣲣����ѹ�
      *****************************************************************
      ******************************************************************
      *    ���Уɴ�����̾����             (xml2)
      *                                   COPY (CPDISEASEV3REQ.INC)
      ******************************************************************
       01  XML-DISEASEREQ.
           03  DISEASEQ-CONTEXT            PIC S9(9)   BINARY.
           03  DISEASEQ-OBJECT             PIC X(36).
           03  DISEASEQ-MODE               PIC S9(9)   BINARY.
           03  DISEASEQ-RECORDNAME         PIC X(128).
           03  DISEASEQ-PATIENTREQ.
               05  DISEASEQ-PATIENT-ID     PIC X(20).
               05  DISEASEQ-BASE-MONTH     PIC X(7).
               05  DISEASEQ-PERFORM-DATE   PIC X(10).
               05  DISEASEQ-PERFORM-TIME   PIC X(8).
               05  DISEASEQ-DEPARTMENT-CODE
                                           PIC X(2).
      *
               05  DISEASEQ-INFORMATION        OCCURS  50.
      *            �ݸ���ʬ�ʣ������ݰʳ���
                   07   DISEASEQ-INSURANCE-CLASS
                                               PIC X(1).
                   07  DISEASEQ-SPICOD1        PIC X(155).
                   07  DISEASEQ-SPINAME1       PIC X(80).
                   07  DISEASEQ-SNDSPITBL.
                       09  DISEASEQ-SNDSPITBL-OCC  OCCURS  21.
                           11   DISEASEQ-SPICOD2   PIC X(7).
                           11   DISEASEQ-SPINAME2  PIC X(20).
      *            ��­������
                   07   DISEASEQ-HOSOKU-COMMENT
                                               PIC X(40).
      *            ��­�����ȥ�����
                   07   DISEASEQ-HOSOKUCD-G.
                       09   DISEASEQ-HOSOKUCD  PIC X(10)
                                                   OCCURS  3.
      *            ������ʬ
                   07   DISEASEQ-NYUGAIKBN     PIC X(1).
      *            ����
                   07   DISEASEQ-SYUBYOFLG     PIC X(2).
      *            �����ե饰
                   07   DISEASEQ-UTAGAIFLG     PIC X(1).
      *            �����ե饰
                   07   DISEASEQ-KYUSEIFLG     PIC X(1).
      *            ����������
                   07   DISEASEQ-STARTDATE     PIC X(10).
      *            ������λ��
                   07   DISEASEQ-ENDDATE       PIC X(10).
      *            ž��
                   07   DISEASEQ-TENKIKBN      PIC X(2).
      *            �������̾
                   07   DISEASEQ-KARTE-NAME    PIC X(80).
      *            ������ʬ
                   07   DISEASEQ-MANSEIKBN     PIC X(4).
      *            �ݸ�Ŭ��
                   07   DISEASEQ-HKNCOMBI      PIC X(4).
      *            �쥻�ץ�ɽ��
                   07   DISEASEQ-REZEFLG       PIC X(4).
      *            ɽ������
                   07   DISEASEQ-REZEMM        PIC X(4).
      *            �ݸ���̾
                   07   DISEASEQ-HKNBYOMEIFLG  PIC X(4).
      *            �ౡ������
                   07   DISEASEQ-DISCHARGEFLG  PIC X(4).
      *            ��������ʬ
                   07   DISEASEQ-CLASS1        PIC X(4).
      *            ��ʻ�ɶ�ʬ
                   07   DISEASEQ-CLASS2        PIC X(4).
      *
