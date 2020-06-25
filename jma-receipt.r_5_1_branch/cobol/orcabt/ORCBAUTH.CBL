      *****************************************************************
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
      *****************************************************************
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCBAUTH.
      *****************************************************************
      *  �����ƥ�̾        : �ϣңã�
      *  ���֥����ƥ�̾    : ���åȥ��å�
      *  ����ݡ��ͥ��̾  : �ѥ���ɥ����å�
      *  ������            :
      *  ��������    ��ȼ�        ����
      *  14/08/07    NACL�ݰ�ƣ��   ��������
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
      *
       DATA                        DIVISION.
      *
       WORKING-STORAGE             SECTION.
      *
       01  WRK-AREA.
           05  WRK-PASSWORD1           PIC X(128).
           05  WRK-PASSWORD2           PIC X(128).
      *
      *****************************************************************
      *    �ե�����쥤������
      *****************************************************************
      *
           COPY    "CPORCSAUTH.INC".
      *
      *****************************************************************
       PROCEDURE                   DIVISION.
      ****************************************************************
      *    �硡������
      *****************************************************************
       000-MAIN                    SECTION.
      *
           INITIALIZE  WRK-AREA
      *
           ACCEPT  WRK-PASSWORD1   FROM  ENVIRONMENT "PASSWORD1"
           ACCEPT  WRK-PASSWORD2   FROM  ENVIRONMENT "PASSWORD2"
      *
           INITIALIZE  ORCSAUTHAREA
           MOVE    "auth"              TO  AUTH-COMMAND
           MOVE    1                   TO  AUTH-ID
           MOVE    WRK-PASSWORD1       TO  AUTH-PASSWORD1
           MOVE    WRK-PASSWORD2       TO  AUTH-PASSWORD2
           CALL    "orcsauth"      USING   ORCSAUTHAREA
           MOVE    AUTH-RC             TO  RETURN-CODE
           .
       000-MAIN-EXT.
           EXIT    PROGRAM.
      *