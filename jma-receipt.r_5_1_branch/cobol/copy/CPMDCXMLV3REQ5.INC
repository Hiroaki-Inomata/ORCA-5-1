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
      *    HAORI�ꥯ�����ȡ���ϥӥꥳ���������Ͼ��� (���β�)
      *                                     COPY (CPMDCXMLV3REQ5.INC)
      ******************************************************************
       01  XML-MEDICALV3REQ5.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       �ꥯ�������ֹ�
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID���Żҥ���ơ�
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       �������
      *****   05  MDCREQ-BASE-DATE         PIC X(10).
      *       �����ֹ�
              05  MDCREQ-PATIENTID         PIC X(20).
      *       UID�ʥ��륫��
              05  MDCREQ-ORCA-UID          PIC X(36).
      *
      *       ���������򡦹������ơʥꥯ�����ȡᣰ������
              05  MDCREQ-SANTEIRRK-INFO.
      *           ������ʬ��NEW,MODIFY,DELETE,����)
                  07  MDCREQ-SANTEI-MODE           PIC X(6).
      *           ���������ť�����
                  07  MDCREQ-PRSCRPT-CODE          PIC X(9).
      *           ����̾��
                  07  MDCREQ-PRSCRPT-NAME          PIC X(80).
      *           ����ǯ��
                  07  MDCREQ-PERFORM-YM            PIC X(07).
      *           �ݸ��ȹ礻�ֹ�
                  07  MDCREQ-COMBINATION-NUMBER    PIC X(04).
      *           ������
                  07  MDCREQ-PERFORM-DATE-G.
                      09  MDCREQ-PERFORM-DAY       PIC X(02)
                                                   OCCURS   31.
      *
      *       �����ȡʥꥯ�����ȡᣰ����
              05  MDCREQ-COMMNT-INFO.
      *           ������ʬ��MODIFY)(����������ν�����ʬ�������Τߡ�
                  07  MDCREQ-COMMNT-MODE           PIC X(6).
      *           ������
                  07  MDCREQ-COMMNT-DAY-TBL        OCCURS   31.
      *               ������
                      09  MDCREQ-COMMNT-DAY            PIC X(02).
      *               ��λ��
                      09  MDCREQ-END-DATE              PIC X(10).
      *               �����ȣ�
                      09  MDCREQ-COMMNT-TBL-G.
                          11  MDCREQ-COMMNT-TBL        OCCURS   50.
                              13  MDCREQ-COMMNT        PIC X(80).
                              13  MDCREQ-COM-END-DATE  PIC X(10).
      *
      *