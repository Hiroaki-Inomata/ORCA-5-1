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
       IDENTIFICATION                  DIVISION.
       PROGRAM-ID.                     ORCSPREF.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ����
      *  ����ݡ��ͥ��̾  : ��ƻ�ܸ�̾����
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  08/11/23    NACL������    ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                     DIVISION.
       CONFIGURATION                   SECTION.
       INPUT-OUTPUT                    SECTION.
       FILE-CONTROL.
      *
       DATA                            DIVISION.
       FILE                            SECTION.
      *
      *
       WORKING-STORAGE                 SECTION.
      *
       01  FLG-AREA.
           03  FLG-ERR                 PIC 9(01).
      *
       01  IDX-AREA.
           03  IDX0                    PIC 9(05).
      *
       01  WRK-AREA.
           03  WRK-DUMMY               PIC X(01).
      *
       01  CONST-AREA.
           03  CONST-PREF-G.
             05  CONST-PREF01 PIC X(22) VALUE "01�̳�ƻ    �̳�ƻ    ".
             05  CONST-PREF02 PIC X(22) VALUE "02�Ŀ���    �Ŀ�      ".
             05  CONST-PREF03 PIC X(22) VALUE "03��긩    ���      ".
             05  CONST-PREF04 PIC X(22) VALUE "04�ܾ븩    �ܾ�      ".
             05  CONST-PREF05 PIC X(22) VALUE "05���ĸ�    ����      ".
             05  CONST-PREF06 PIC X(22) VALUE "06������    ����      ".
             05  CONST-PREF07 PIC X(22) VALUE "07ʡ�縩    ʡ��      ".
             05  CONST-PREF08 PIC X(22) VALUE "08��븩    ���      ".
             05  CONST-PREF09 PIC X(22) VALUE "09���ڸ�    ����      ".
             05  CONST-PREF10 PIC X(22) VALUE "10���ϸ�    ����      ".
             05  CONST-PREF11 PIC X(22) VALUE "11��̸�    ���      ".
             05  CONST-PREF12 PIC X(22) VALUE "12���ո�    ����      ".
             05  CONST-PREF13 PIC X(22) VALUE "13�����    �����    ".
             05  CONST-PREF14 PIC X(22) VALUE "14�����  ������    ".
             05  CONST-PREF15 PIC X(22) VALUE "15���㸩    ����      ".
             05  CONST-PREF16 PIC X(22) VALUE "16�ٻ���    �ٻ�      ".
             05  CONST-PREF17 PIC X(22) VALUE "17���    ����      ".
             05  CONST-PREF18 PIC X(22) VALUE "18ʡ�温    ʡ��      ".
             05  CONST-PREF19 PIC X(22) VALUE "19������    ����      ".
             05  CONST-PREF20 PIC X(22) VALUE "20Ĺ�    Ĺ��      ".
             05  CONST-PREF21 PIC X(22) VALUE "21���츩    ����      ".
             05  CONST-PREF22 PIC X(22) VALUE "22�Ų���    �Ų�      ".
             05  CONST-PREF23 PIC X(22) VALUE "23���θ�    ����      ".
             05  CONST-PREF24 PIC X(22) VALUE "24���Ÿ�    ����      ".
             05  CONST-PREF25 PIC X(22) VALUE "25���츩    ����      ".
             05  CONST-PREF26 PIC X(22) VALUE "26������    ������    ".
             05  CONST-PREF27 PIC X(22) VALUE "27�����    �����    ".
             05  CONST-PREF28 PIC X(22) VALUE "28ʼ�˸�    ʼ��      ".
             05  CONST-PREF29 PIC X(22) VALUE "29���ɸ�    ����      ".
             05  CONST-PREF30 PIC X(22) VALUE "30�²λ���  �²λ�    ".
             05  CONST-PREF31 PIC X(22) VALUE "31Ļ�踩    Ļ��      ".
             05  CONST-PREF32 PIC X(22) VALUE "32�纬��    �纬      ".
             05  CONST-PREF33 PIC X(22) VALUE "33������    ����      ".
             05  CONST-PREF34 PIC X(22) VALUE "34���縩    ����      ".
             05  CONST-PREF35 PIC X(22) VALUE "35������    ����      ".
             05  CONST-PREF36 PIC X(22) VALUE "36���縩    ����      ".
             05  CONST-PREF37 PIC X(22) VALUE "37���    ����      ".
             05  CONST-PREF38 PIC X(22) VALUE "38��ɲ��    ��ɲ      ".
             05  CONST-PREF39 PIC X(22) VALUE "39���θ�    ����      ".
             05  CONST-PREF40 PIC X(22) VALUE "40ʡ����    ʡ��      ".
             05  CONST-PREF41 PIC X(22) VALUE "41���츩    ����      ".
             05  CONST-PREF42 PIC X(22) VALUE "42Ĺ�긩    Ĺ��      ".
             05  CONST-PREF43 PIC X(22) VALUE "43���ܸ�    ����      ".
             05  CONST-PREF44 PIC X(22) VALUE "44��ʬ��    ��ʬ      ".
             05  CONST-PREF45 PIC X(22) VALUE "45�ܺ긩    �ܺ�      ".
             05  CONST-PREF46 PIC X(22) VALUE "46�����縩  ������    ".
             05  CONST-PREF47 PIC X(22) VALUE "47���츩    ����      ".
           03    CONST-PREF-R    REDEFINES  CONST-PREF-G.
             05  CONST-PREF-OCC         OCCURS 47.
                 07  CONST-PREFNUM      PIC X(02).
                 07  CONST-PREFNAME1    PIC X(10).
                 07  CONST-PREFNAME2    PIC X(10).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
      *****************************************************************
      *    ���֥ץ����ΰ�
      *****************************************************************
      *
      *
      ****************************************************************
       LINKAGE                         SECTION.
           COPY    "CPORCSPREF.INC".
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       PROCEDURE                       DIVISION
           USING
           SPREF-AREA
           SPA-AREA
           .
      *
      *****************************************************************
      *    �����
      *****************************************************************
       000-PROC-SEC                    SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT PROGRAM
      *
           .
      *****************************************************************
      *    ���������
      *****************************************************************
       100-INIT-SEC                    SECTION.
      *
           INITIALIZE                  IDX-AREA
                                       FLG-AREA
                                       WRK-AREA
      *
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    �����
      *****************************************************************
       200-MAIN-SEC                    SECTION.
      *
           PERFORM VARYING IDX0    FROM    1   BY  1
                   UNTIL ( IDX0    >   47 )
      *
               MOVE    CONST-PREFNUM (IDX0)
                                   TO  SPREF-PREFNUM (IDX0)
               MOVE    CONST-PREFNAME1(IDX0)
                                   TO  SPREF-PREFNAME1(IDX0)
               MOVE    CONST-PREFNAME2(IDX0)
                                   TO  SPREF-PREFNAME2(IDX0)
      *
           END-PERFORM
      *
           .
       200-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    ��λ����
      *****************************************************************
       300-END-SEC                     SECTION.
      *
      *
           .
       300-END-EXT.
           EXIT.
