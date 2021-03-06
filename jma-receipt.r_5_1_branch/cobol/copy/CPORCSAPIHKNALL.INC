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
      ************************************************************
      *    ���У����ݸ��ȹ礻�Խ��ѥ�᥿����COPY (CPORCSAPIHKNALL.INC)
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 05.00.00     NACL-¿��Ǽ  20/04/XX  ���ݸ��Ծڻ����ɲ�
      ************************************************************
       01  ORCSAPIHKNALLAREA.
      *����
           03  ORCSAPIHKN-PTID             PIC 9(10).
      *������ʬ�ʣ����������դΤ��оݡ���ô����������դǷ����
           03  ORCSAPIHKN-KBN              PIC X(01).
      *��������
           03  ORCSAPIHKN-SYSYMD           PIC X(08).
      *��������ǯ��
           03  ORCSAPIHKN-START-YM         PIC X(06).
      *������λǯ��
           03  ORCSAPIHKN-END-YM           PIC X(06).
      *��ǯ�����ʽ�����ʬ�ᣱ�Τߡ�
           03  ORCSAPIHKN-BIRTHDAY         PIC X(08).
      *
      *�����ΰ�
      *ͭ���ݸ�����
           03  ORCSAPIHKN-HKNCOMBI-AREA.
               05  ORCSAPIHKN-HKNCOMBI-TBL     OCCURS   200.
      *            �ݸ��ȹ礻
                   07  ORCSAPIHKN-THKNCOMBI        PIC  X(04).
      *            �ݸ��ȹ礻��ô���ʳ����
                   07  ORCSAPIHKN-COMB-RATEG       PIC  X(04).
      *            �ݸ��ȹ礻��ô����������
                   07  ORCSAPIHKN-COMB-RATEN       PIC  X(04).
      *            �ݸ��ȹ礻ͭ������
                   07  ORCSAPIHKN-COMB-TEKSTYMD    PIC  X(10).
                   07  ORCSAPIHKN-COMB-TEKEDYMD    PIC  X(10).
      *
                   07  ORCSAPIHKN-COMBN-TEKSTYMD   PIC  X(08).
                   07  ORCSAPIHKN-COMBN-TEKEDYMD   PIC  X(08).
      *
      *            �ݸ�����
                   07  ORCSAPIHKN-HKNNUM           PIC  X(03).
      *            �ݸ�����̾��
                   07  ORCSAPIHKN-HKNNUM-NAME      PIC  X(20).
      *            �ݸ����ֹ�
                   07  ORCSAPIHKN-HKNJANUM         PIC  X(08).
      *            ����
                   07  ORCSAPIHKN-KIGO             PIC  X(80).
      *            �ֹ�
                   07  ORCSAPIHKN-NUM              PIC  X(80).
      *R02.4       �ֹ����
                   07  ORCSAPIHKN-EDABAN           PIC  X(02).
      *            ��³��ʬ
                   07  ORCSAPIHKN-CONTKBN          PIC  X(01).
      *            �����ʬ
                   07  ORCSAPIHKN-HOJOKBN          PIC  X(01).
      *            �����ʬ̾��
                   07  ORCSAPIHKN-HOJOKBN-NAME     PIC  X(30).
      *            �ܿͲ�²��ʬ
                   07  ORCSAPIHKN-HONKZKKBN        PIC  X(01).
      *            ���ݸ���̾��
                   07  ORCSAPIHKN-HIHKNJANAME      PIC  X(100).
      *            Ŭ�Ѵ��֡�ɽ����
                   07  ORCSAPIHKN-TEKSTYMD         PIC  X(10).
                   07  ORCSAPIHKN-TEKEDYMD         PIC  X(10).
      *            Ŭ�Ѵ���
                   07  ORCSAPIHKN-NAI-TEKSTYMD     PIC  X(08).
                   07  ORCSAPIHKN-NAI-TEKEDYMD     PIC  X(08).
      *            ��ʼ�����
                   07  ORCSAPIHKN-SKKGETYMD        PIC  X(10).
                   07  ORCSAPIHKN-NAI-SKKGETYMD    PIC  X(08).
      *            ��ǧ����
                   07  ORCSAPIHKN-KAKUNINYMD       PIC  X(10).
                   07  ORCSAPIHKN-NAI-KAKUNINYMD   PIC  X(08).
      *            �ݸ��ɣ�
                   07  ORCSAPIHKN-NAI-HKNID        PIC  9(10).
      *            ϫ�Ҷ�ʬ
                   07  ORCSAPIHKN-RSI-HKNKBN       PIC  X(01).
      *            ��ɽ����ʬ
                   07  ORCSAPIHKN-HYOJIKBN         PIC  X(01).
      *            ������ࡦ����ɣ�
                   07  ORCSAPIHKN-KOH-G.
                       09  ORCSAPIHKN-KOH-OCC      OCCURS   4.
                           11  ORCSAPIHKN-KOHNUM           PIC  X(03).
                           11  ORCSAPIHKN-KOHNUM-NAME      PIC  X(20).
                           11  ORCSAPIHKN-FTNJANUM         PIC  X(08).
                           11  ORCSAPIHKN-JKYSNUM          PIC  X(20).
      *                    Ŭ�Ѵ���
                           11  ORCSAPIHKN-KOH-TEKSTYMD     PIC  X(10).
                           11  ORCSAPIHKN-KOH-TEKEDYMD     PIC  X(10).
      *R01.6
      *                    ��ǧ����
                           11  ORCSAPIHKN-KOH-KAKUNINYMD   PIC  X(10).
      *                    ��������ôΨ
                           11  ORCSAPIHKN-RATE-ADMISSION   PIC X(05).
      *                    �����ݸ����
                           11  ORCSAPIHKN-MONEY-ADMISSION  PIC X(06).
      *                    �������ôΨ
                           11  ORCSAPIHKN-RATE-OUTPATIENT  PIC X(05).
      *                     ����ݸ����
                           11  ORCSAPIHKN-MONEY-OUTPATIENT PIC X(06).
      *                    ����ɣ�
                           11  ORCSAPIHKN-NAI-KOHID        PIC 9(10).
      *
      *            ϫ���ݸ�����
                   07  ORCSAPIHKN-RSIINF-AREA.
      *                ϫ���ݸ�̾�Ρʵ����Ʊ�����ơ�
                       09  ORCSAPIHKN-ACCIDENT-NAME        PIC X(50).
      *                ���¤����̡��ֹ��Ʊ�����ơ�
                       09  ORCSAPIHKN-DISEASE-LOCATION     PIC X(100).
      *                ����ǯ����
                       09  ORCSAPIHKN-DISEASE-DATE         PIC X(10).
      *ϫ��
      *                ϫƯ�ݸ��ֹ�
                       09  ORCSAPIHKN-ACCIDENT-NUMBER      PIC X(14).
      *                ǯ��ڽ��ֹ�
                       09  ORCSAPIHKN-PENSIONCERTIFICATE   PIC X(09).
      *                �ҳ���ʬ
                       09  ORCSAPIHKN-ACCIDENT-CLASS       PIC X(01).
      *                �ҳ���ʬ��̾�Ρ�
                       09  ORCSAPIHKN-ACCIDENT-CLASS-NAME  PIC X(20).
      *                ϫƯ�����Ľ𥳡���
                       09  ORCSAPIHKN-LABOR-ST-CODE        PIC X(05).
      *                ϫƯ�����Ľ𥳡���̾��
                       09  ORCSAPIHKN-LABOR-ST-NAME        PIC X(50).
      *
      *                ���Ѻ��̶�ʬ
                       09  ORCSAPIHKN-SINKEI               PIC X(01).
                       09  ORCSAPIHKN-SINKEI-NAME          PIC X(10).
      *                ž����ͳ
                       09  ORCSAPIHKN-TENKI                PIC X(01).
                       09  ORCSAPIHKN-TENKI-NAME           PIC X(10).
      *                �ͻ������ʬ
                       09  ORCSAPIHKN-SISIKBN              PIC X(01).
                       09  ORCSAPIHKN-SISIKBN-NAME         PIC X(10).
      *
      *                ���Ƚ�̾��
                       09  ORCSAPIHKN-RSI-JIGYOUNAME       PIC X(80).
      *                �������ƻ�ܸ�̾
                       09  ORCSAPIHKN-RSI-PREFNAME         PIC X(20).
      *                �������ƻ�ܸ���ʬ
                       09  ORCSAPIHKN-RSI-PREFKBN          PIC X(01).
      *                �������ƻ�ܸ���ʬ̾��
                       09  ORCSAPIHKN-RSI-PREFKBNNAME      PIC X(02).
      *                ����Ϸ��Զ�̾
                       09  ORCSAPIHKN-RSI-CITYNAME         PIC X(80).
      *                ����Ϸ��Զ��ʬ
                       09  ORCSAPIHKN-RSI-CITYKBN          PIC X(01).
      *                ����Ϸ��Զ�̾��
                       09  ORCSAPIHKN-RSI-CITYKBNNAME      PIC X(02).
      *                ϫ�ҥ쥻������� ���ǯ��
                       09  ORCSAPIHKN-KIJYUNYM             PIC X(07).
      *                ϫ�ҥ쥻������� ���
                       09  ORCSAPIHKN-RESEPUTO-COUNT       PIC X(03).
      *������
      *                �����������ʬ
                       09  ORCSAPIHKN-JIBAISEIKBN          PIC X(01).
                       09  ORCSAPIHKN-JIBAISEIKBN-NAME     PIC X(20).
      *                �������ݸ����̾
                       09  ORCSAPIHKN-RSI-HOKENNAME        PIC X(80).
      *                ������ô����
                       09  ORCSAPIHKN-RSI-DRCD             PIC X(05).
      *                ������ô����̾��
                       09  ORCSAPIHKN-RSI-DRCD-NAME        PIC X(100).
      *���ե�������
      *                �򹯴�����Ģ�ֹ�
                       09  ORCSAPIHKN-RSI-KANRITCHO-NUMBER PIC X(13).
      *                »����ʬ
                       09  ORCSAPIHKN-DAMAGE-CLASS         PIC X(03).
      *                »����ʬ̾��
                       09  ORCSAPIHKN-DAMAGE-CLASS-NAME    PIC X(100).
      *�軰�Թ԰�
      *                ��ʪ�ٵ��ʬ
                       09  ORCSAPIHKN-DS-GENBTKBN          PIC X(01).
                       09  ORCSAPIHKN-DS-GENBTKBN-NAME     PIC X(20).
      *                �õ������ʬ
                       09  ORCSAPIHKN-DS-TOKKIKBN          PIC X(01).
                       09  ORCSAPIHKN-DS-TOKKIKBN-NAME     PIC X(20).
      *
      *        �о��ݸ��ȹ礻���
               05  ORCSAPIHKN-OT-MAX           PIC 9(04).
      *���顼������
           03  ORCSAPIHKN-ERRCD                    PIC 9(01).
      *
      *
