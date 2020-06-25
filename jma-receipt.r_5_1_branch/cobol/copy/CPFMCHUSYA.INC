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
      ******************************************************************
      *    ���Уɡ��������ͻؼ��Ģɼ����   COPY (CPFMCHUSYA.INC)
      ******************************************************************
        01  FMCHUSYA-REC.
            03  FMCHUSYA-CONTEXT                PIC S9(9)   BINARY.
            03  FMCHUSYA-OBJECT                 PIC X(36).
            03  FMCHUSYA-MODE                   PIC S9(9)   BINARY.
            03  FMCHUSYA-RECORDNAME             PIC X(128).
            03  FMCHUSYA-DATA.
              05  FMCHUSYA-FORM-ID              PIC X(30).
              05  FMCHUSYA-CUSTOM-ID            PIC X(30).
              05  FMCHUSYA-PRINTER              PIC X(20).
      *       Ģɼ�����ȥ�
              05  FMCHUSYA-TITLE-AREA.
                  07  FMCHUSYA-TITLE-OCC        OCCURS 4.
                      09  FMCHUSYA-TITLE        PIC X(30).
                      09  FMCHUSYA-MAISU        PIC X(2).
              05  FMCHUSYA-HAKKOKBN             PIC X(1).
      *       ������
              05  FMCHUSYA-SRYYMD               PIC X(10).
      *       �ؼ���
              05  FMCHUSYA-SYSYMD               PIC X(10).
      *       �ؼ�������
              05  FMCHUSYA-SYSTIME              PIC X(5).
      *       ���Ųʡ����쥻��
              05  FMCHUSYA-SRYKA                PIC X(2).
              05  FMCHUSYA-SRYKANAME            PIC X(40).
      *       �����ֹ�
              05  FMCHUSYA-BTUNUM               PIC X(2).
              05  FMCHUSYA-BTUNAME              PIC X(20).
              05  FMCHUSYA-BRMNUM               PIC X(6).
      *       ���Ծ���
              05  FMCHUSYA-PTINF.
                07  FMCHUSYA-PTNUM              PIC X(20).
                07  FMCHUSYA-NAME               PIC X(50).
                07  FMCHUSYA-KANANAME           PIC X(50).
                07  FMCHUSYA-BIRTHDAY           PIC X(10).
                07  FMCHUSYA-SEX                PIC X(1).
      *       �ɥ�����
              05  FMCHUSYA-DOCTOR-AREA.
                07  FMCHUSYA-DR-CODE             PIC X(5).
                07  FMCHUSYA-DR-NAME             PIC X(50).
                07  FMCHUSYA-DR-KANANAME         PIC X(50).
                07  FMCHUSYA-DR-MAYAKULICENSE    PIC X(15).
      *
      *       1:�����򵭺ܤ����ؼ�����
              05  FMCHUSYA-MAYAKU                PIC X(1).
      *
              05  FMCHUSYA-ZAI-AREA.
                07  FMCHUSYA-SRYKBN              PIC X(2).
      *         ���ż��̶�ʬ
                07  FMCHUSYA-SRYSYUKBN           PIC X(3).
      *         ����̾��
                07  FMCHUSYA-CHUSYA-NAME         PIC X(100).
      *         �굻�������ɡʤ�����Τߡ�
                07  FMCHUSYA-CHUSYA-SRYCD        PIC X(9).
      *         ���Ϣ��
                07  FMCHUSYA-KAI-RENNUM          PIC X(2).
      *
                07  FMCHUSYA-MEDICATION          OCCURS  50 TIMES.
      *           ����̾
                  09  FMCHUSYA-YKZNAME           PIC X(200).
      *           ����
                  09  FMCHUSYA-SURYO             PIC X(12).
      *           ñ��̾��
                  09  FMCHUSYA-TANINAME          PIC X(12).
      *           ������
                  09  FMCHUSYA-SRYCD             PIC X(9).
              05  FMCHUSYA-MEMO                  PIC X(110)
                                                 OCCURS  3  TIMES.
              05  FMCHUSYA-OPID                  PIC X(16).