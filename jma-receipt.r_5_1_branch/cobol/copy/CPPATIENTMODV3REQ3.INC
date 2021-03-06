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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
      *****************************************************************
      *  �ץ�����ཤ������
      * Maj/Min/Rev  ������       ����      ����
      *****************************************************************
      ******************************************************************
      *    ���Уɴ�����Ͽ�����ϫ���ݸ������(���β���V3)
      *                                   COPY (CPPATIENTMODV3REQ3.INC)
      ******************************************************************
        01  XML-PATIENTMODREQ.
            02  PTMODREQ-CONTEXT           PIC S9(9)   BINARY.
            02  PTMODREQ-OBJECT            PIC X(36).
            02  PTMODREQ-MODE              PIC S9(9)   BINARY.
            02  PTMODREQ-RECORDNAME        PIC X(128).
            02  PTMODREQ-MEDICALREQ.
      *       �ꥯ�������ֹ�
              03  PTMODREQ-REQUEST-NUMBER   PIC X(2).
      *       UID���Żҥ���ơ�
              03  PTMODREQ-KARTE-UID        PIC X(36).
      *       UID�ʥ��륫��
              03  PTMODREQ-ORCA-UID         PIC X(36).
      *       ������աʥ����ƥ����ա�
              03  PTMODREQ-BASE-DATE        PIC X(10).
      *       ��ǧ��å���������
              03  PTMODREQ-SELECT-ANSWER    PIC X(03).
      *       ��³��ʬ
              03  PTMODREQ-CONT-MODE        PIC X(05).
      *       ���Ծ���
              03  PTMODREQ-PTINF-INFO.
      *         �����ֹ�
                05  PTMODREQ-PATIENTID        PIC X(20).
      *         ���Ի�̾
                05  PTMODREQ-NAME             PIC X(100).
      *         ���ԥ��ʻ�̾
                05  PTMODREQ-KANANAME         PIC X(100).
      *         ��ǯ����
                05  PTMODREQ-BIRTHDAY         PIC X(10).
      *         ����
                05  PTMODREQ-SEX              PIC X(01).
      *
      *        ϫ���ݸ�����
              03  PTMODREQ-ACCIDENT-INFO-G.
                 05  PTMODREQ-ACCIDENT-INFO     OCCURS   40.
      *            ������ʬ
                   07  PTMODREQ-MAIN-MODE              PIC X(06).
      *            ϫ���ݸ��ɣ�
                   07  PTMODREQ-MAIN-INSURANCE-ID      PIC X(10).
      *            ����ɣġ��軰�Թ԰١�
                   07  PTMODREQ-INSURANCE-ID           PIC X(10).
      *            �ݸ��μ���
                   07  PTMODREQ-INSURANCE-CLASS        PIC X(03).
      *            �ݸ��μ���̾��
                   07  PTMODREQ-INSURANCE-NAME         PIC X(20).
      *ϫ�Ҿ���
      *            ϫ���ݸ���ʬ
                   07  PTMODREQ-ACCIDENT              PIC X(01).
      *            ϫ���ݸ�̾��
                   07  PTMODREQ-ACCIDENT-NAME          PIC X(50).
      *            ���¤�����
                   07  PTMODREQ-DISEASE-LOCATION       PIC X(100).
      *            ����ǯ����
                   07  PTMODREQ-DISEASE-DATE           PIC X(10).
      *            ���ܳ�����
                   07  PTMODREQ-START-DATE             PIC X(10).
      *            ���ܽ�λ��
                   07  PTMODREQ-END-DATE               PIC X(10).
      *ϫ��
      *            ϫƯ�ݸ��ֹ�
                   07  PTMODREQ-ACCIDENT-NUMBER        PIC X(14).
      *            ǯ��ڽ��ֹ�
                   07  PTMODREQ-PENSIONCERTIFICATE     PIC X(09).
      *            �ҳ���ʬ
                   07  PTMODREQ-ACCIDENT-CLASS         PIC X(01).
      **           �ҳ���ʬ̾��
      **           07  PTMODREQ-ACCIDENT-CLASS-NAME    PIC X(20).
      *            ϫƯ�����Ľ𥳡���
                   07  PTMODREQ-LABOR-ST-CODE          PIC X(05).
      *            ϫƯ�����Ľ𥳡���
      **           07  PTMODREQ-LABOR-ST-NAME          PIC X(50).
      *
      *            ���Ѻ��̶�ʬ
                   07  PTMODREQ-SINKEI                 PIC X(01).
      **           07  PTMODREQ-SINKEI-NAME            PIC X(10).
      *            ž����ͳ
                   07  PTMODREQ-TENKI                  PIC X(01).
      **           07  PTMODREQ-TENKI-NAME             PIC X(10).
      *            �ͻ������ʬ
                   07  PTMODREQ-SISIKBN                PIC X(01).
      **           07  PTMODREQ-SISIKBN-NAME           PIC X(10).
      *
      *            ���Ƚ�̾��
                   07  PTMODREQ-JIGYOUNAME             PIC X(80).
      *            �������ƻ�ܸ�̾
                   07  PTMODREQ-PREFNAME               PIC X(20).
      *            �������ƻ�ܸ���ʬ
                   07  PTMODREQ-PREFKBN                PIC X(01).
                   07  PTMODREQ-PREFKBN-NAME           PIC X(02).
      *            ����Ϸ��Զ�̾
                   07  PTMODREQ-CITYNAME               PIC X(80).
      *            ����Ϸ��Զ��ʬ
                   07  PTMODREQ-CITYKBN                PIC X(01).
      *            ����Ϸ��Զ�̾��
                   07  PTMODREQ-CITYKBN-NAME           PIC X(02).
      *            ϫ�ҥ쥻������� ���ǯ��
                   07  PTMODREQ-KIJYUNYM               PIC X(07).
      *            ϫ�ҥ쥻������� ���
                   07  PTMODREQ-RESEPUTO-COUNT         PIC X(03).
      *������
      *            �����������ʬ
                   07  PTMODREQ-JIBAISEIKBN            PIC X(01).
      *            �������ݸ����̾
                   07  PTMODREQ-HOKENNAME              PIC X(80).
      *            ������ô����
                   07  PTMODREQ-RSI-DRCD               PIC X(05).
      *            �����ն�ʬ
                   07  PTMODREQ-JIBAIKBN               PIC  X(01).
      *���ե�������
      *            �򹯴�����Ģ�ֹ�
                   07  PTMODREQ-KANRITCHO-NUMBER       PIC X(13).
      *            »����ʬ
                   07  PTMODREQ-DAMAGE-CLASS           PIC X(03).
      *            »����ʬ̾��
      **           07  PTMODREQ-DAMAGE-CLASS-NAME      PIC X(100).
      *�軰�Թ԰�
      *            ��ʪ�ٵ��ʬ
                   07  PTMODREQ-DS-GENBTKBN            PIC X(01).
      **           07  PTMODREQ-DS-GENBTKBN-NAME       PIC X(20).
      *            �õ������ʬ
                   07  PTMODREQ-DS-TOKKIKBN            PIC X(01).
      **           07  PTMODREQ-DS-TOKKIKBN-NAME       PIC X(20).
      *
      *
