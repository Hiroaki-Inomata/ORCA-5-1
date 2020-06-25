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
      *    ���Уɥꥯ�����������ޥ�����Ͽ     COPY (CPACCPTLSTV2REQ.INC)
      ******************************************************************
        01  XML-MEDICATIONREQ.
            02  MEDIREQ-CONTEXT            PIC S9(9)   BINARY.
            02  MEDIREQ-OBJECT             PIC X(36).
            02  MEDIREQ-MODE               PIC S9(9)   BINARY.
            02  MEDIREQ-RECORDNAME         PIC X(128).
            02  MEDIREQ-APPOINT.
      *       �ޥ���������
              03  MEDIREQ-MEDICATION-CODE      PIC X(09).
      *       ����̾��
              03  MEDIREQ-MEDICATION-NAME      PIC X(100).
      *       ����̾��
              03  MEDIREQ-MEDICATION-KNAME     PIC X(100).
      *       ������
              03  MEDIREQ-START-DATE           PIC X(10).
      *       ��λ��
              03  MEDIREQ-END-DATE             PIC X(10).
      *       ������
              03  MEDIREQ-AMOUNT-MONEY         PIC X(9).
      *       ���񥳡����ΰ�
              03  MEDIREQ-AMOUNT-AREA.
      *           ������ơ�4:�ǹ��ߡ�
                  05  MEDIREQ-CONTENT-MONEY        PIC X(1).
      *           ������ʳ����
                  05  MEDIREQ-TOTAL-OUT            PIC X(3).
      *           �������������
                  05  MEDIREQ-TOTAL-IN             PIC X(3).
      *           �����ս�����ʳ����
                  05  MEDIREQ-LIABILITY-TOTAL-OUT  PIC X(1).
      *           �����ս������������
                  05  MEDIREQ-LIABILITY-TOTAL-IN   PIC X(1).
      *       ���̥������ΰ�
              03  MEDIREQ-LOCATION-AREA.
      *           ���̶�ʬ
                  05  MEDIREQ-LOCATION-CATEGORY    PIC X(1).
      *       �����ȥ������ΰ�
              03  MEDIREQ-COMMENT-AREA.
      *           ��������
                  05  MEDIREQ-COMMENT-OCC      OCCURS   5.
                      07  MEDIREQ-COLUMN-POSITION  PIC X(3).
                      07  MEDIREQ-DIGIT-NUMBER     PIC X(3).
      *       ��ˡ�������ΰ�
              03  MEDIREQ-MEDICATION-AREA.
      *           ���ѻ���
                  05  MEDIREQ-MEDICATION-OCC      OCCURS   5.
                      07  MEDIREQ-MEDICATION-POINT  PIC X(1).
      *           ��ˡ�����ȶ�ʬ
                  05  MEDIREQ-MEDICATION-CATEGORY   PIC X(1).
      *       �����ॳ�����ΰ�
              03  MEDIREQ-EQUIPMENT-AREA.
      *           ñ�̥�����
                  05  MEDIREQ-UNIT-CODE            PIC X(3).
      *           �ǡ�����ʬ��3:�ե�����
                  05  MEDIREQ-DATA-CATEGORY        PIC X(1).
      *       ��ྦ��̾�������ΰ�
              03  MEDIREQ-COMMERCIAL-AREA.
      *           ����̾��
                  05  MEDIREQ-COMMERCIALNAME       PIC X(200).
      *           �����ॳ����
                  05  MEDIREQ-EQUIPMENT-CODE       PIC X(9).