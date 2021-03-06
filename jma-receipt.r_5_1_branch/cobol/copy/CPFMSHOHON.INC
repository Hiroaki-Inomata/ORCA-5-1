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
      *    ���Уɡ����������Ģɼ����        COPY (CPFMSHOHON.INC)
      ******************************************************************
        01  FMSHOHON-REC.
            03  FMSHOHON-CONTEXT                PIC S9(9)   BINARY.
            03  FMSHOHON-OBJECT                 PIC X(36).
            03  FMSHOHON-MODE                   PIC S9(9)   BINARY.
            03  FMSHOHON-RECORDNAME             PIC X(128).
            03  FMSHOHON-DATA.
              05  FMSHOHON-FORM-ID              PIC X(30).
              05  FMSHOHON-CUSTOM-ID            PIC X(30).
              05  FMSHOHON-PRINTER              PIC X(20).
      *       Ģɼ�����ȥ�
              05  FMSHOHON-TITLE-AREA.
                  07  FMSHOHON-TITLE-OCC        OCCURS 4.
                      09  FMSHOHON-TITLE        PIC X(30).
                      09  FMSHOHON-MAISU        PIC X(2).
              05  FMSHOHON-HAKKOKBN             PIC X(1).
      *       ������
              05  FMSHOHON-SRYYMD               PIC X(10).
      *       ������
              05  FMSHOHON-SYSYMD               PIC X(10).
      *       ���ջ���
              05  FMSHOHON-SYSTIME              PIC X(5).
      *       ���Ųʡ����쥻��
              05  FMSHOHON-SRYKA                PIC X(2).
              05  FMSHOHON-SRYKANAME            PIC X(20).
      *       �����ֹ�
              05  FMSHOHON-BTUNUM               PIC X(2).
              05  FMSHOHON-BTUNAME              PIC X(20).
              05  FMSHOHON-BRMNUM               PIC X(6).
      *       ���Ծ���
              05  FMSHOHON-PTINF.
                07  FMSHOHON-PTNUM              PIC X(20).
                07  FMSHOHON-NAME               PIC X(50).
                07  FMSHOHON-KANANAME           PIC X(50).
                07  FMSHOHON-BIRTHDAY           PIC X(10).
                07  FMSHOHON-SEX                PIC X(1).
      *       �ɥ�����
              05  FMSHOHON-DOCTOR-AREA.
                07  FMSHOHON-DR-CODE             PIC X(5).
                07  FMSHOHON-DR-NAME             PIC X(50).
                07  FMSHOHON-DR-KANANAME         PIC X(50).
                07  FMSHOHON-DR-MAYAKULICENSE    PIC X(15).
      *
      *       1:�����򵭺ܤ����ؼ�����
              05  FMSHOHON-MAYAKU                PIC X(1).
      *       1:�׻�
              05  FMSHOHON-RINJI                 PIC X(1).
      *
              05  FMSHOHON-ZAI-AREA              OCCURS  50 TIMES.
                07  FMSHOHON-SRYKBN              PIC X(2).
      *
                07  FMSHOHON-ZAIKAISU-G          OCCURS   31.
                    09  FMSHOHON-DAY             PIC X(02).
                    09  FMSHOHON-KAISU           PIC X(03).
      *
                07  FMSHOHON-MEDICATION          OCCURS  50 TIMES.
      *           ����̾
                  09  FMSHOHON-YKZNAME           PIC X(200).
      *           ����
                  09  FMSHOHON-SURYO             PIC X(12).
      *           ñ��̾��
                  09  FMSHOHON-TANINAME          PIC X(12).
      *           ������
                  09  FMSHOHON-SRYCD             PIC X(9).
              05  FMSHOHON-MEMO                  PIC X(110)
                                                 OCCURS  3  TIMES.
              05  FMSHOHON-OPID                  PIC X(16).
              05  FMSHOHON-PARA-PAGECNT          PIC X(1).
