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
      *****************************************************************
      ******************************************************************
      *    ���Уɥ쥹�ݥ�ʻ�Ѷش����ޥ����å�����
      *                                   COPY (CPCONTRACHECKV2RES.INC)
      ******************************************************************
       01  XML-CONTRA-CHECKV2RES.
            03  CHKRES-CONTEXT             PIC S9(9)   BINARY.
            03  CHKRES-OBJECT              PIC X(36).
            03  CHKRES-MODE                PIC S9(9)   BINARY.
            03  CHKRES-RECORDNAME          PIC X(128).
            03  CHKRES-MEDICALRES.
      *        �»���
               05  CHKRES-INFORMATION-DATE  PIC X(10).
      *        �»ܻ���
               05  CHKRES-INFORMATION-TIME  PIC X(8).
      *        �ֵѥ�����
               05  CHKRES-API-RESULT        PIC X(2).
      *        �ֵѥ�å�����
               05  CHKRES-API-RESULT-MSG    PIC X(100).
      *
               05  CHKRES-RESKEY            PIC X(100).
      *        ����ǯ��
               05  CHKRES-PERFORM-YM        PIC X(07).
      *        ���Ծ���
               05  CHKRES-PATIENTID        PIC X(20).
               05  CHKRES-NAME             PIC X(100).
               05  CHKRES-KANANAME         PIC X(100).
               05  CHKRES-BIRTHDAY         PIC X(10).
               05  CHKRES-SEX              PIC X(1).
      *
      *        �����å������ֵ�
               05  CHKRES-MEDICAL-INF-G.
                   07  CHKRES-MDC-INFO          OCCURS   30.
      *                ���ť�����
                       09  CHKRES-PRSCRPT-CODE          PIC X(9).
      *                ����̾��
                       09  CHKRES-PRSCRPT-NAME          PIC X(80).
      *                ���顼��ʬ
                       09  CHKRES-PRSCRPT-ERRCD         PIC X(02).
      *                ���顼��å�����
                       09  CHKRES-PRSCRPT-ERRMSG        PIC X(80).
      *
      *                �ش�����
                       09  CHKRES-INTERACT-INFO-G.
                           10  CHKRES-INTERACT-INFO    OCCURS   30.
      *                        �ش���������
                               11  CHKRES-DRUGCD-CODE      PIC X(9).
      *                        �ش���̾��
                               11  CHKRES-DRUGCD-NAME      PIC X(80).
      *                        �ɾ��ܵ���ʬ
                               11  CHKRES-INTERACT-CODE    PIC X(07).
      *                        ��Ϳ��
                               11  CHKRES-ADMINISTE-DATE   PIC X(10).
      *                        ����ʬ
                               11  CHKRES-ADMINISTE-KBN    PIC X(01).
      *                        ������
                               11  CHKRES-CONTEXT-CLASS    PIC X(06).
      *
      *        �ɾ��ܵ������ֵ�
               05  CHKRES-SSKIJYO-INF-G.
                   07  CHKRES-SSKIJYO-INFO           OCCURS   50.
      *                �ɾ��ܵ���ʬ��INTERACT-CODE��
                       09  CHKRES-SYOJYOUCD           PIC X(07).
      *                �ɾ�
                       09  CHKRES-SYOJYOU             PIC X(2048).
      *                ���ѵ�Ϳ
                       09  CHKRES-SAYOKIJYO           PIC X(2048).
      *
