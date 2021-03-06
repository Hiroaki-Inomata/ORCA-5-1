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
      *    ���Уɥ쥹�ݥ󥹿��Ųʾ������
      *                                     COPY (CPDEPARTMETV2RES.INC)
      ******************************************************************
        01  XML-DEPARTMENTRES.
            02  DEPTRES-CONTEXT            PIC S9(9)   BINARY.
            02  DEPTRES-OBJECT             PIC X(36).
            02  DEPTRES-MODE               PIC S9(9)   BINARY.
            02  DEPTRES-RECORDNAME         PIC X(128).
            02  DEPTRES-APPOINTRES.
      *       �»���
              03  DEPTRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  DEPTRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  DEPTRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  DEPTRES-API-RESULT-MSG      PIC X(100).
      *
              03  DEPTRES-RESKEY              PIC X(100).
      *       �����
              03  DEPTRES-BASE-DATE           PIC X(10).
      *       ���Ųʰ���
              03  DEPTRES-DEPARTMET-INFOMATION.
                 05  DEPTRES-DEPARTMET-INFO      OCCURS   100.
      *               ���Ųʥ�����
                      07  DEPTRES-DEP-CODE        PIC X(2).
      *               ���Ų�̾��
                      07  DEPTRES-SRYKANAME       PIC X(80).
      *               û��̾�Σ�
                      07  DEPTRES-SRYKANAME1      PIC X(20).
      *               û��̾�Σ�
                      07  DEPTRES-SRYKANAME2      PIC X(12).
      *               û��̾�Σ�
                      07  DEPTRES-SRYKANAME3      PIC X(04).
      *               �쥻�ſ��Ųʥ�����
                      07  DEPTRES-RECESRYKA       PIC X(02).
      *
