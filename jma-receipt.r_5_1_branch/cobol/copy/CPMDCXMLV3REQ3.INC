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
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  19/01/XX  ʣ�����ݸ��б�
      *****************************************************************
      ******************************************************************
      *    ���Уɥꥯ�����ȿ��Ź԰���Ͽ���� (���β�)
      *                                       COPY (CPMDCXMLV3REQ3.INC)
      *         ���Ź԰�����
      ******************************************************************
       01  XML-MEDICALV3REQ3.
           03  MDCREQ-CONTEXT          PIC S9(9)   BINARY.
           03  MDCREQ-OBJECT           PIC X(36).
           03  MDCREQ-MODE             PIC S9(9)   BINARY.
           03  MDCREQ-RECORDNAME       PIC X(128).
           03  MDCREQ-MEDICALREQ.
      *       �ꥯ�������ֹ�
              05  MDCREQ-REQUEST-NUMBER    PIC X(2).
      *       UID���Żҥ���ơ�
              05  MDCREQ-KARTE-UID         PIC X(36).
      *       ������աʼ�Ǽ��������
              05  MDCREQ-BASE-DATE         PIC X(10).
      *       �����ֹ�
              05  MDCREQ-PATIENTID         PIC X(20).
      *       ��������
              05  MDCREQ-PERFORM-DATE      PIC X(10).
      *       ���Ż���
              05  MDCREQ-PERFORM-TIME      PIC X(8).
      *       UID�ʥ��륫��
              05  MDCREQ-ORCA-UID          PIC X(36).
      *H27.12
      *       ������ʬ
              05  MDCREQ-PATIENT-MODE      PIC X(06).
      *
      *       ��ưȯ���޺����ʬ�ʣ�����������
              05  MDCREQ-MDC-MODE          PIC X(01).
      *       ��ưȯ���޺��Ϣ��
              05  MDCREQ-DEL-NUMBER-G.
                  07  MDCREQ-DEL-NUMBER        PIC X(02)
                                               OCCURS   30.
      *H29.7
      *       ��Ͽ������ʬ��Finish:��Ͽ�ˡʥꥯ�������ֹ�=05)
              05  MDCREQ-REQUEST-MODE      PIC X(06).
      *
      *       ������ˡ
              05  MDCREQ-IC-CODE           PIC X(02).
      *H29.7
      *       �����갷����ʬ
              05  MDCREQ-IC-REQUEST        PIC X(01).
      *       ������ֶ��ʬ
              05  MDCREQ-IC-ALLKBN         PIC X(01).
      *
      *       ��Ǽ����
              05  MDCREQ-SYUNOU-AREA.
      *!!!!
      *2019.1
      *ʣ���ݸ������б�
                  07  MDCREQ-SYUNOU-OCCURS       OCCURS   15.
      *               �ݸ��ȹ礻�ֹ�
                      09  MDCREQ-CD-COMBINATION-NUMBER    PIC X(04).
      *               ���Ų�
                      09  MDCREQ-CD-DEP-CODE      PIC X(02).
      *               ����ۡ�������
                      09  MDCREQ-CD-IC-MONEY      PIC X(10).
      *               Ĵ����ۣ�
                      09  MDCREQ-CD-AD-MONEY1     PIC X(10).
      *               Ĵ����ۣ�
                      09  MDCREQ-CD-AD-MONEY2     PIC X(10).
      *!!!!
      *           Ĵ����ۣ���ñ�ȤΤߡ�
                  07  MDCREQ-AD-MONEY1         PIC X(10).
      *           Ĵ����ۣ���ñ�ȤΤߡ�
                  07  MDCREQ-AD-MONEY2         PIC X(10).
      *           ����ۡʹ�פȰ㤦���Τߡ�
                  07  MDCREQ-IC-MONEY          PIC X(10).
      *           �ֶ�ۡ������Τߡ��������ֶ�ϥ�����
                  07  MDCREQ-RE-MONEY          PIC X(10).
      *!!!!
      *       �����⡼�� 2019/09/19 Added
              05  MDCREQ-PRINT-MODE            PIC X(10).
      *H30.XX
              05  MDCREQ-PRINT-INIT-AREA.
      *           �������μ��񡦿��������ٽ�ȯ�Զ�ʬ
                  07  MDCREQ-HAKKOU-KBN        PIC X(01).
      *!!!!
      *H29.6
      *   Ģɼ�����ؼ���ʬ
              05  MDCREQ-PRINT-AREA.
      *           �����������
                  07  MDCREQ-PRINT-SHOHOU      PIC X(01).
      *           �������μ������
                  07  MDCREQ-PRINT-SEIKYU      PIC X(01).
      *           ���������ٽ����
                  07  MDCREQ-PRINT-MEISAI      PIC X(01).
      *           ���޾������
                  07  MDCREQ-PRINT-YAKUJYO     PIC X(01).
      *           ������Ģ����
                  07  MDCREQ-PRINT-TECHOU      PIC X(01).
      *           ͽ��ɼ����
                  07  MDCREQ-PRINT-YOYAKU      PIC X(01).
      *