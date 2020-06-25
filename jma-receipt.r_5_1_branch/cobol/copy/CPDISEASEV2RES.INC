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
      *****************************************************************
      ******************************************************************
      *    ���Уɴ�����̾����             (xml2)
      *                                   COPY (CPDISEASEV3RES.INC)
      ******************************************************************
       01  XML-DISEASERES.
           03  DISEASES-CONTEXT            PIC S9(9)   BINARY.
           03  DISEASES-OBJECT             PIC X(36).
           03  DISEASES-MODE               PIC S9(9)   BINARY.
           03  DISEASES-RECORDNAME         PIC X(128).
           03  DISEASES-DISEASE-RES.
      *        �»���
               05  DISEASES-INFORMATION-DATE
                                           PIC X(10).
      *        �»ܻ���
               05  DISEASES-INFORMATION-TIME
                                           PIC X(8).
      *        �ֵѥ�����
               05  DISEASES-API-RESULT     PIC X(3).
      *        �ֵѥ�å�����
               05  DISEASES-API-RESULT-MSG PIC X(100).
      *
               05  DISEASES-RESKEY         PIC X(100).
      *
               05  DISEASES-PERFORM-DATE   PIC X(10).
               05  DISEASES-PERFORM-TIME   PIC X(8).
               05  DISEASES-DPERTMENT-CODE PIC X(2).
               05  DISEASES-DPERTMENT-NAME PIC X(80).
               05  DISEASES-PATIENT-ID     PIC X(20).
               05  DISEASES-BASE-MONTH     PIC X(7).
               05  DISEASES-DIAG-TBL-G     OCCURS   50.
      *            ���顼������
                   07  DISEASES-RESULT         PIC X(3).
      *            ��å�����
                   07  DISEASES-RESULT-MSG     PIC X(100).
       *            �ٹ𥳡���
                   07  DISEASES-DIAG-WARNING   PIC X(03).
      *            ��å�����
                   07  DISEASES-DIAG-MSG       PIC X(100).
      *            ���ֹ�
                   07  DISEASES-DIAG-POSITION  PIC X(02).
      *            ����������
                   07   DISEASES-DIAG-STARTDATE
                                               PIC X(10).
      *            ��̾
                   07  DISEASES-DIAG-NAME      PIC X(80).
      *            ��̾������
                   07  DISEASES-DIAG-CODE      PIC X(50).
      *            �ܹ�(�ѻ�)
                   07  DISEASES-DIAG-CHANGE    PIC X(02).
      *        �԰�����̾����
               05  DISEASES-UNMATCH-INFORMATION.
      *            �԰�����̾���󥪡��С��ե饰
                   07  DISEASES-INFORMATION-OVERFLOW
                                               PIC X(5).
      *            �԰�����̾����
                   07  DISEASES-UNMATCH-INFORMATION    OCCURS  50.
                       09  DISEASES-SPICOD1    PIC X(50).
                       09  DISEASES-SPINAME1   PIC X(80).
      *                ��­�����ȥ�����
                       09  DISEASES-HOSOKUCD-G.
                           11  DISEASES-HOSOKUCD
                                               PIC X(10)   OCCURS  3.
      *                ��­������
                       09  DISEASES-HOSOKU-COMMENT
                                               PIC X(40).
      *                ������ʬ
                       09  DISEASES-NYUGAIKBN  PIC X(1).
      *                ����
                       09  DISEASES-SYUOBYOFLG PIC X(2).
      *                �����ե饰
                       09  DISEASES-UTAGAIFLG  PIC X(1).
      *                ����������
                       09  DISEASES-STARTDATE  PIC X(10).
      *                ������λ��
                       09  DISEASES-ENDDATE    PIC X(10).
      *                ž��
                       09  DISEASES-TENKIKBN   PIC X(1).
      *                �������̾
                       09  DISEASES-KARTE-NAME PIC X(80).
      *                ������ʬ
                       09  DISEASES-MANSEIKBN  PIC X(2).
      *                �ݸ�Ŭ��
                       09  DISEASES-HKNCOMBI   PIC X(4).
      *                �쥻�ץ�ɽ��
                       09  DISEASES-REZEFLG    PIC X(1).
      *                ɽ������
                       09  DISEASES-REZEMM     PIC X(2).
      *                �ݸ���̾
                       09  DISEASES-HKNBYOMEIFLG
                                               PIC X(1).
      *                �ౡ������
                       09  DISEASES-DISCHARGEFLG
                                               PIC X(1).
      *                ��������ʬ
                       09  DISEASES-CLASS1     PIC X(2).
      *                ��ʻ�ɶ�ʬ
                       09  DISEASES-CLASS2     PIC X(2).