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
        01  FMMEISAI-REC.
            03  FMMEISAI-CONTEXT               PIC S9(9)   BINARY.
            03  FMMEISAI-OBJECT                PIC X(36).
            03  FMMEISAI-MODE                  PIC S9(9)   BINARY.
            03  FMMEISAI-RECORDNAME            PIC X(128).
            03  FMMEISAI-DATA.
              05  FMMEISAI-FORM-ID           PIC X(30).
              05  FMMEISAI-CUSTOM-ID           PIC X(30).
              05  FMMEISAI-PRINTER             PIC X(20).
      *
      *       �����ֹ�
              05  FMMEISAI-PTNUM             PIC X(20).
      *       ���ԥ��ʻ�̾
              05  FMMEISAI-KANANAME          PIC X(50).
      *       ���Ի�̾
              05  FMMEISAI-NAME              PIC X(50).
      *
      *       ���ŵ��ؾ���
              05  FMMEISAI-HOSP-AREA.
      *         ���ŵ���̾��
                07  FMMEISAI-HOSP-NAME-G.
                  09  FMMEISAI-HOSP-NAME       PIC X(120)
                                               OCCURS   3.
      *         ���ŵ���͹���ֹ�
                07  FMMEISAI-HOSP-POST         PIC X(7).
      *         ���ŵ��ؽ���
                07  FMMEISAI-HOSP-ADRS-G.
                  09  FMMEISAI-HOSP-ADRS       PIC X(200)
                                               OCCURS   3.
      *         ���ŵ��������ֹ�
                07  FMMEISAI-HOSP-TEL          PIC X(15).
      *
      *       ���Ͽ�����
              05  FMMEISAI-STSRYYMD          PIC X(10).
      *       ��λ������
              05  FMMEISAI-EDSRYYMD          PIC X(10).
      *       ��ɼȯ����
              05  FMMEISAI-DENPPRTYMD        PIC X(10).
      *
      *       ���Ų�
              05  FMMEISAI-SRYKANAME         PIC X(30).
      *       �ݸ�����
              05  FMMEISAI-HOKENNAME         PIC X(40).
              05  FMMEISAI-FUTAN-G.
      *         ��ô���
                07  FMMEISAI-FUTAN         PIC X(04).
      *         ��ô���
                07  FMMEISAI-FUTANMEI      PIC X(02).
      *       ��ɼ�ֹ�ʰ��ʳ���
              05  FMMEISAI-DENPNUM           PIC X(07).
      *
      *       ���پ���
              05   FMMEISAI-SRY-ITEM-AREA  OCCURS   2000.
      *         ���롼�ץإå��ʣ�����ɼ�ֹ椬�Ѥ���٤���Ƭ��
                07   FMMEISAI-GRPHEAD        PIC X(01).
      *         Ϣ�֡���ɼ�ֹ椬�Ѥ���٤����áˡ�999����ɼ����ԡ�
                07   FMMEISAI-GRPNUM         PIC X(03).
      *         ���Ŷ�ʬ̾
                07   FMMEISAI-SRYKBN         PIC X(10).
      *         ̾��
                07  FMMEISAI-SRYNAME-G.
      *           �޳��ϡʡ��ʤ�
                  09  FMMEISAI-ZAIHEN        PIC X(02).
      *           ��������̾��
                  09  FMMEISAI-SRYNAME       PIC X(148).
      *         �������ܱ�Ω�ƥե饰
                07   FMMEISAI-TENSU-MARK     PIC X(1).
      *         ����
                07   FMMEISAI-TENSU          PIC X(09).
      *         ���
                07   FMMEISAI-KAISU          PIC X(03).
      *         ���
                07   FMMEISAI-TOTAL          PIC X(11).
      *         �޽�λ�ʣ����޺ǽ��ԡ�
                07  FMMEISAI-ZAIEND          PIC X(01).
      *
      *       ���پ������Խ��ե饰
              05  FMMEISAI-YEN-FLG           PIC X(1).
      *
      *       ���ս�
              05  FMMEISAI-CAUTION           PIC X(100).
