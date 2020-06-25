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
      *    ���Уɥ��ݥ������ޥ�����Ͽ     COPY (CPMEDICATIONV2RES.INC)
      ******************************************************************
        01  XML-MEDICATIONRES.
            02  MEDIRES-CONTEXT            PIC S9(9)   BINARY.
            02  MEDIRES-OBJECT             PIC X(36).
            02  MEDIRES-MODE               PIC S9(9)   BINARY.
            02  MEDIRES-RECORDNAME         PIC X(128).
            02  MEDIRES-APPOINTRES.
      *       �»���
              03  MEDIRES-INFORMATION-DATE    PIC X(10).
      *       �»ܻ���
              03  MEDIRES-INFORMATION-TIME    PIC X(8).
      *       �ֵѥ�����
              03  MEDIRES-API-RESULT          PIC X(2).
      *       �ֵѥ�å�����
              03  MEDIRES-API-RESULT-MSG      PIC X(100).
      *
              03  MEDIRES-RESKEY              PIC X(100).
      *
      *       �ޥ���������
              03  MEDIRES-MEDICATION-CODE      PIC X(09).
      *       ����̾��
              03  MEDIRES-MEDICATION-NAME      PIC X(100).
      *       ����̾��
              03  MEDIRES-MEDICATION-KNAME     PIC X(100).
      *       ������
              03  MEDIRES-START-DATE           PIC X(10).
      *       ��λ��
              03  MEDIRES-END-DATE             PIC X(10).
      *       ��ۡʼ��񡦴���
              03  MEDIRES-AMOUNT-MONEY         PIC X(9).
      *       ���񥳡����ΰ�
              03  MEDIRES-AMOUNT-AREA.
      *           ������ơ�4:�ǹ��ߡ�
                  05  MEDIRES-CONTENT-MONEY        PIC X(1).
      *           ������ʳ����
                  05  MEDIRES-TOTAL-OUT            PIC X(3).
      *           �������������
                  05  MEDIRES-TOTAL-IN             PIC X(3).
      *           �����ս�����ʳ����
                  05  MEDIRES-LIABILITY-TOTAL-OUT  PIC X(1).
      *           �����ս������������
                  05  MEDIRES-LIABILITY-TOTAL-IN   PIC X(1).
      *       ���̥������ΰ�
              03  MEDIRES-LOCATION-AREA.
      *           ���̶�ʬ
                  05  MEDIRES-LOCATION-CATEGORY    PIC X(1).
      *       �����ȥ������ΰ�
              03  MEDIRES-COMMENT-AREA.
      *           ��������
                  05  MEDIRES-COMMENT-OCC      OCCURS   5.
                      07  MEDIRES-COLUMN-POSITION  PIC X(3).
                      07  MEDIRES-DIGIT-NUMBER     PIC X(3).
      *       ��ˡ�������ΰ�
              03  MEDIRES-MEDICATION-AREA.
      *           ���ѻ���
                  05  MEDIRES-MEDICATION-OCC      OCCURS   5.
                      07  MEDIRES-MEDICATION-POINT  PIC X(1).
      *           ��ˡ�����ȶ�ʬ
                  05  MEDIRES-MEDICATION-CATEGORY   PIC X(1).
      *       �����ॳ�����ΰ�
              03  MEDIRES-EQUIPMENT-AREA.
      *           ñ�̥�����
                  05  MEDIRES-UNIT-CODE            PIC X(3).
      *           �ǡ�����ʬ��3:�ե�����
                  05  MEDIRES-DATA-CATEGORY        PIC X(1).
      *       ��ྦ��̾�������ΰ�
              03  MEDIRES-COMMERCIAL-AREA.
      *           ����̾��
                  05  MEDIRES-COMMERCIALNAME       PIC X(200).
      *           �����ॳ����
                  05  MEDIRES-EQUIPMENT-CODE       PIC X(9).