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
        01  FMMEISAIN-REC.
            03  FMMEISAIN-CONTEXT              PIC S9(9)   BINARY.
            03  FMMEISAIN-OBJECT               PIC X(36).
            03  FMMEISAIN-MODE                 PIC S9(9)   BINARY.
            03  FMMEISAIN-RECORDNAME           PIC X(128).
            03  FMMEISAIN-DATA.
              05  FMMEISAIN-FORM-ID            PIC X(30).
              05  FMMEISAIN-CUSTOM-ID          PIC X(30).
              05  FMMEISAIN-PRINTER            PIC X(20).
      *
      *       �����ֹ�
              05  FMMEISAIN-PTNUM             PIC X(20).
      *       ���Ի�̾
              05  FMMEISAIN-NAME              PIC X(50).
      *       ���ԥ��ʻ�̾
              05  FMMEISAIN-KANANAME          PIC X(50).
      *
      *       ���ŵ��ؾ���
              05  FMMEISAIN-HOSP-AREA.
      *         ���ŵ���̾��
                07  FMMEISAIN-HOSP-NAME-G.
                  09  FMMEISAIN-HOSP-NAME      PIC X(120)
                                               OCCURS   3.
      *         ���ŵ���͹���ֹ�
                07  FMMEISAIN-HOSP-POST        PIC X(7).
      *         ���ŵ��ؽ���
                07  FMMEISAIN-HOSP-ADRS-G.
                  09  FMMEISAIN-HOSP-ADRS      PIC X(200)
                                               OCCURS   3.
      *         ���ŵ��������ֹ�
                07  FMMEISAIN-HOSP-TEL         PIC X(15).
      *
      *       ���Ͽ�����
              05  FMMEISAIN-STSRYYMD           PIC X(10).
      *       ��λ������
              05  FMMEISAIN-EDSRYYMD           PIC X(10).
      *       ��ɼȯ����
              05  FMMEISAIN-DENPPRTYMD         PIC X(10).
      *       �����ֹ�
              05  FMMEISAIN-BTUNUM             PIC X(2).
      *       ����̾��
              05  FMMEISAIN-BTUNAME            PIC X(20).
      *       �¼��ֹ�
              05  FMMEISAIN-BRMNUM             PIC X(6).
      *       ���Ų�
              05  FMMEISAIN-SRYKANAME          PIC X(30).
      *       �ݸ�����
              05  FMMEISAIN-HOKENNAME          PIC X(40).
              05  FMMEISAIN-FUTAN-G.
      *         ��ô���
                07  FMMEISAIN-FUTAN            PIC X(04).
      *         ��ô���
                07  FMMEISAIN-FUTANMEI         PIC X(02).
      *       ��ɼ�ֹ�ʰ��ʳ���
              05  FMMEISAIN-DENPNUM            PIC X(07).
      *
      *       ���پ���
              05   FMMEISAIN-SRY-ITEM-AREA     OCCURS   2000.
      *         ���롼�ץإå��ʣ�����ɼ�ֹ椬�Ѥ���٤���Ƭ��
                07   FMMEISAIN-GRPHEAD         PIC X(01).
      *         Ϣ�֡���ɼ�ֹ椬�Ѥ���٤����áˡ�999����ɼ����ԡ�
                07   FMMEISAIN-GRPNUM          PIC X(03).
      *         ���Ŷ�ʬ̾
                07   FMMEISAIN-SRYKBN          PIC X(10).
      *         ̾��
                07  FMMEISAIN-SRYNAME-G.
      *           �޳��ϡʡ��ʤ�
                  09  FMMEISAIN-ZAIHEN         PIC X(02).
      *           ��������̾��
                  09  FMMEISAIN-SRYNAME        PIC X(148).
      *         �������ܱ�Ω�ƥե饰
                07   FMMEISAIN-TENSU-MARK      PIC X(1).
      *         ����
                07   FMMEISAIN-TENSU           PIC X(09).
      *         ���
                07   FMMEISAIN-KAISU           PIC X(03).
      *         ���
                07   FMMEISAIN-TOTAL           PIC X(11).
      *         �޽�λ�ʣ����޺ǽ��ԡ�
                07  FMMEISAIN-ZAIEND           PIC X(01).
      *
      *       ���پ������Խ��ե饰
              05  FMMEISAIN-YEN-FLG            PIC X(1).
      *
      *       ���ս�
              05  FMMEISAIN-CAUTION            PIC X(100).
