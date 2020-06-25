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
      *         �����ƥ�����ޥ���     (CPSK2010.INC)            *
      *         �ʣ���������                                     *
      *         �����ղþ���                                     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  SYS-2010-REC.
           03  SYS-2010-KEY.
               05  SYS-2010-HOSPNUM         PIC  9(02).
      *����������
               05  SYS-2010-KANRICD         PIC  X(04).
      *��ʬ������
               05  SYS-2010-KBNCD           PIC  X(08).
      *ͭ������ǯ����
               05  SYS-2010-STYUKYMD        PIC  X(08).
      *ͭ����λǯ����
               05  SYS-2010-EDYUKYMD        PIC  X(08).
      *
      *��������ά����
           03  SYS-2010-JNISSUKBN           PIC  X(01).
           03  SYS-2010-JNISSU-TAIKBN       PIC  X(01).
      *��������ά����
           03  SYS-2010-TOTALTENKBN         PIC  X(01).
           03  SYS-2010-TOTALTEN-TAIKBN     PIC  X(01).
      *������ô���ά����
           03  SYS-2010-FTNMONEYKBN-G       PIC  X(01).
           03  SYS-2010-FTNMONEY-TAIKBN-G   PIC  X(01).
           03  SYS-2010-FTNMONEYKBN-N       PIC  X(01).
           03  SYS-2010-FTNMONEY-TAIKBN-N   PIC  X(01).
      *������ô�⣰�ߵ��ܡʵ�Ͽ��
           03  SYS-2010-FTNZEROKBN-G        PIC  X(01).
           03  SYS-2010-FTNZERO-TAIKBN-G    PIC  X(01).
           03  SYS-2010-FTNZEROKBN-N        PIC  X(01).
           03  SYS-2010-FTNZERO-TAIKBN-N    PIC  X(01).
      *�����������ܾ�ά����
           03  SYS-2010-SKJKBN              PIC  X(01).
           03  SYS-2010-SKJ-TAIKBN          PIC  X(01).
      *�ϣãҼ�������������ά����
           03  SYS-2010-OCRKBN              PIC  X(01).
           03  SYS-2010-OCR-TAIKBN          PIC  X(01).
      *���������Ծ��ѹ�����ô�׻�
           03  SYS-2010-JYUHENKBN           PIC  X(01).
           03  SYS-2010-JYUHEN-TAIKBN       PIC  X(01).
      *��������Ʒ��󵭺�
           03  SYS-2010-SAIKEIKBN           PIC  X(01).
           03  SYS-2010-SAIKEI-TAIKBN       PIC  X(01).
      *�������ʻ��ʬ����������
           03  SYS-2010-SYUKEIKBN           PIC  X(01).
           03  SYS-2010-SYUKEI-TAIKBN       PIC  X(01).
      *�������³���ô�׻�
           03  SYS-2010-JGNFTNKBN           PIC  X(01).
           03  SYS-2010-JGNFTN-TAIKBN       PIC  X(01).
      *
      *������ô���۽���
           03  SYS-2010-FTNKEIKBN-G         PIC  X(01).
           03  SYS-2010-FTNKEI-TAIKBN-G     PIC  X(01).
           03  SYS-2010-FTNKEIKBN-N         PIC  X(01).
           03  SYS-2010-FTNKEI-TAIKBN-N     PIC  X(01).
      *�ݸ���ݰ�����ô�⵭�ܡʹ���ԡ�
           03  SYS-2010-HKNFTNKBN-K         PIC  X(01).
           03  SYS-2010-HKNFTN-TAIKBN-K     PIC  X(01).
      *�ݸ���ݰ�����ô�⵭�ܡ�70��̤����
           03  SYS-2010-HKNFTNKBN-Y         PIC  X(01).
           03  SYS-2010-HKNFTN-TAIKBN-Y     PIC  X(01).
      *����Ȱۤʤ뵭������
           03  SYS-2010-RECEKSIKBN-G        PIC  X(01).
           03  SYS-2010-RECEKSI-TAIKBN-G    PIC  X(01).
           03  SYS-2010-RECEKSIKBN-N        PIC  X(01).
           03  SYS-2010-RECEKSI-TAIKBN-N    PIC  X(01).
      *�����å��оݾ�³�����
           03  SYS-2010-CHKJGNKBN-G         PIC  X(01).
           03  SYS-2010-CHKJGN-TAIKBN-G     PIC  X(01).
           03  SYS-2010-CHKJGNKBN-N         PIC  X(01).
           03  SYS-2010-CHKJGN-TAIKBN-N     PIC  X(01).
      *��������ơ��֥����
           03  SYS-2010-KOHSKYKBN-G         PIC  X(01).
           03  SYS-2010-KOHSKY-TAIKBN-G     PIC  X(01).
           03  SYS-2010-KOHSKYKBN-N         PIC  X(01).
           03  SYS-2010-KOHSKY-TAIKBN-N     PIC  X(01).
      *
      *������Ͽ�ݽ����Ծ���ݷ��³�����
           03  SYS-2010-PTJGNKBN            PIC  X(01).
           03  SYS-2010-PTJGN-TAIKBN        PIC  X(01).
      *�����̤�����ʾ�ǰۤʤ���ô��׻�
           03  SYS-2010-IRYOKEIKBN-G        PIC  X(01).
           03  SYS-2010-IRYOKEI-TAIKBN-G    PIC  X(01).
           03  SYS-2010-IRYOKEIKBN-N        PIC  X(01).
           03  SYS-2010-IRYOKEI-TAIKBN-N    PIC  X(01).
           03  SYS-2010-SKJKEIKBN           PIC  X(01).
           03  SYS-2010-SKJKEI-TAIKBN       PIC  X(01).
      *�ܿͥ��֡��������ܡ��������ܡʿ����ˡ��������ܡʴĶ���
           03  SYS-2010-SKJRYOKBN-HON       PIC  X(01).
           03  SYS-2010-SKJRYO-TAIKBN-HON   PIC  X(01).
           03  SYS-2010-SEIRYOKBN-HON       PIC  X(01).
           03  SYS-2010-SEIRYO-TAIKBN-HON   PIC  X(01).
           03  SYS-2010-SEIKANKBN-HON       PIC  X(01).
           03  SYS-2010-SEIKAN-TAIKBN-HON   PIC  X(01).
      *��������֡��������ܡ��������ܡʿ����ˡ��������ܡʴĶ���
           03  SYS-2010-SKJRYOKBN-TSY       PIC  X(01).
           03  SYS-2010-SKJRYO-TAIKBN-TSY   PIC  X(01).
           03  SYS-2010-SEIRYOKBN-TSY       PIC  X(01).
           03  SYS-2010-SEIRYO-TAIKBN-TSY   PIC  X(01).
           03  SYS-2010-SEIKANKBN-TSY       PIC  X(01).
           03  SYS-2010-SEIKAN-TAIKBN-TSY   PIC  X(01).
      *��ǯ�⥿�֡��������ܡ��������ܡʿ����ˡ��������ܡʴĶ���
           03  SYS-2010-SKJRYOKBN-TNK       PIC  X(01).
           03  SYS-2010-SKJRYO-TAIKBN-TNK   PIC  X(01).
           03  SYS-2010-SEIRYOKBN-TNK       PIC  X(01).
           03  SYS-2010-SEIRYO-TAIKBN-TNK   PIC  X(01).
           03  SYS-2010-SEIKANKBN-TNK       PIC  X(01).
           03  SYS-2010-SEIKAN-TAIKBN-TNK   PIC  X(01).
      *
      *�ݸ���γ�̺Ʒǵ��ܡʹ���ԡ�
           03  SYS-2010-KAKKBN-K            PIC  X(01).
           03  SYS-2010-KAK-TAIKBN-K        PIC  X(01).
      *�ݸ���γ�̺Ʒǡʲ��˵��ܡʹ���ԡ�
      *����������ϡʡ�Ĺ�ˡ�ʬ������ξ���
           03  SYS-2010-KAK21KBN-K          PIC  X(01).
           03  SYS-2010-KAK21-TAIKBN-K      PIC  X(01).
      *����������ϡ���������ʬ���ʤ��ξ���
           03  SYS-2010-KAK22KBN-K          PIC  X(01).
           03  SYS-2010-KAK22-TAIKBN-K      PIC  X(01).
      *����������ϡ�Ĺ������ʬ���ʤ��ξ���
           03  SYS-2010-KAK23KBN-K          PIC  X(01).
           03  SYS-2010-KAK23-TAIKBN-K      PIC  X(01).
      *�ݸ���γ�̺Ʒǡʲ��˶�۷׻��ʹ���ԡ�
      *����������ϡ���������ʬ������ξ���
           03  SYS-2010-KAK31KBN-K          PIC  X(01).
           03  SYS-2010-KAK31-TAIKBN-K      PIC  X(01).
      *����������ϡ�Ĺ������ʬ������ξ���
           03  SYS-2010-KAK32KBN-K          PIC  X(01).
           03  SYS-2010-KAK32-TAIKBN-K      PIC  X(01).
      *�ݸ���γ�̺Ʒǡʲ��˶�۲û��ʹ���ԡ�
      *����������ϡʡ�Ĺ�ˡ�ʬ������ξ���
           03  SYS-2010-KAK41KBN-K          PIC  X(01).
           03  SYS-2010-KAK41-TAIKBN-K      PIC  X(01).
      *����������ϡ���������ʬ���ʤ��ξ���
           03  SYS-2010-KAK42KBN-K          PIC  X(01).
           03  SYS-2010-KAK42-TAIKBN-K      PIC  X(01).
      *����������ϡ�Ĺ������ʬ���ʤ��ξ���
           03  SYS-2010-KAK43KBN-K          PIC  X(01).
           03  SYS-2010-KAK43-TAIKBN-K      PIC  X(01).
      *
      *�ݸ���γ�̺Ʒǵ��ܡʣ�����̤����
           03  SYS-2010-KAKKBN-Y            PIC  X(01).
           03  SYS-2010-KAK-TAIKBN-Y        PIC  X(01).
      *�ݸ���γ�̺Ʒǡʲ��˵��ܡʣ�����̤����
      *����������ϡʡ�Ĺ�ˡ�ʬ������ξ���
           03  SYS-2010-KAK21KBN-Y          PIC  X(01).
           03  SYS-2010-KAK21-TAIKBN-Y      PIC  X(01).
      *����������ϡ���������ʬ���ʤ��ξ���
           03  SYS-2010-KAK22KBN-Y          PIC  X(01).
           03  SYS-2010-KAK22-TAIKBN-Y      PIC  X(01).
      *����������ϡ�Ĺ������ʬ���ʤ��ξ���
           03  SYS-2010-KAK23KBN-Y          PIC  X(01).
           03  SYS-2010-KAK23-TAIKBN-Y      PIC  X(01).
      *�ݸ���γ�̺Ʒǡʲ��˶�۷׻��ʣ�����̤����
      *����������ϡ���������ʬ������ξ���
           03  SYS-2010-KAK31KBN-Y          PIC  X(01).
           03  SYS-2010-KAK31-TAIKBN-Y      PIC  X(01).
      *����������ϡ�Ĺ������ʬ������ξ���
           03  SYS-2010-KAK32KBN-Y          PIC  X(01).
           03  SYS-2010-KAK32-TAIKBN-Y      PIC  X(01).
      *�ݸ���γ�̺Ʒǡʲ��˶�۲û��ʣ�����̤����
      *����������ϡʡ�Ĺ�ˡ�ʬ������ξ���
           03  SYS-2010-KAK41KBN-Y          PIC  X(01).
           03  SYS-2010-KAK41-TAIKBN-Y      PIC  X(01).
      *����������ϡ���������ʬ���ʤ��ξ���
           03  SYS-2010-KAK42KBN-Y          PIC  X(01).
           03  SYS-2010-KAK42-TAIKBN-Y      PIC  X(01).
      *����������ϡ�Ĺ������ʬ���ʤ��ξ���
           03  SYS-2010-KAK43KBN-Y          PIC  X(01).
           03  SYS-2010-KAK43-TAIKBN-Y      PIC  X(01).
      *
      *�ݸ���ݰ�����ô��̵����
      *���ʹ���ԡ�����
           03  SYS-2010-KSINGKBN-K-G        PIC  X(01).
           03  SYS-2010-KSING-TAIKBN-K-G    PIC  X(01).
      *���ʹ���ԡ�����
           03  SYS-2010-KSINGKBN-K-N        PIC  X(01).
           03  SYS-2010-KSING-TAIKBN-K-N    PIC  X(01).
      *���ʣ���̤������
           03  SYS-2010-KSINGKBN-Y-G        PIC  X(01).
           03  SYS-2010-KSING-TAIKBN-Y-G    PIC  X(01).
      *���ʣ���̤������
           03  SYS-2010-KSINGKBN-Y-N        PIC  X(01).
           03  SYS-2010-KSING-TAIKBN-Y-N    PIC  X(01).
      *������ô�⣱�ߢ�������ñ�̵���
      *���ʹ���ԡ�����
           03  SYS-2010-ENCHGKBN-K-G        PIC  X(01).
           03  SYS-2010-ENCHG-TAIKBN-K-G    PIC  X(01).
      *���ʹ���ԡ�����
           03  SYS-2010-ENCHGKBN-K-N        PIC  X(01).
           03  SYS-2010-ENCHG-TAIKBN-K-N    PIC  X(01).
      *���ʣ���̤������
           03  SYS-2010-ENCHGKBN-Y-G        PIC  X(01).
           03  SYS-2010-ENCHG-TAIKBN-Y-G    PIC  X(01).
      *���ʣ���̤������
           03  SYS-2010-ENCHGKBN-Y-N        PIC  X(01).
           03  SYS-2010-ENCHG-TAIKBN-Y-N    PIC  X(01).
      *�������������ܵ��ܡʸ������֤�������
           03  SYS-2010-KOH2SKJKBN          PIC  X(01).
           03  SYS-2010-KOH2SKJ-TAIKBN      PIC  X(01).
      *
      *��ô��̤ȯ��ʬ�β��������ȡʳ����
           03  SYS-2010-KAICNTKBN-G         PIC  X(01).
           03  SYS-2010-KAICNT-TAIKBN-G     PIC  X(01).
      *��ʪ�����оݼԤ����̷׻��ʹ���ԡ�����
           03  SYS-2010-GNBKEIKBN-K-G       PIC  X(01).
           03  SYS-2010-GNBKEI-TAIKBN-K-G   PIC  X(01).
      *�������ʻ�ѻ������̷׻��ʳ����
           03  SYS-2010-ZENKEIKBN-G         PIC  X(01).
           03  SYS-2010-ZENKEI-TAIKBN-G     PIC  X(01).
      *�ǻ������ʤ�������ô��׻��ʳ����
           03  SYS-2010-SINKEIKBN-G         PIC  X(01).
           03  SYS-2010-SINKEI-TAIKBN-G     PIC  X(01).
      *��ǻ��ʳ�����ô��׻��ʳ����
           03  SYS-2010-SYOKEIKBN-G         PIC  X(01).
           03  SYS-2010-SYOKEI-TAIKBN-G     PIC  X(01).
      *��ǻ��ʳ�����ô��׻���������
           03  SYS-2010-SYOKEIKBN-N         PIC  X(01).
           03  SYS-2010-SYOKEI-TAIKBN-N     PIC  X(01).
      *�������������٤Ȥ������̷׻���������
           03  SYS-2010-NYUKEIKBN-N         PIC  X(01).
           03  SYS-2010-NYUKEI-TAIKBN-N     PIC  X(01).
      *����³ۤ����ۤȤ������̷׻���������
           03  SYS-2010-DAYKEIKBN-N         PIC  X(01).
           03  SYS-2010-DAYKEI-TAIKBN-N     PIC  X(01).
      *���ٳ�ǧ����󼨤�̵���������̷׻�
           03  SYS-2010-NINNASIKBN          PIC  X(01).
           03  SYS-2010-NINNASI-TAIKBN      PIC  X(01).
      *���ٳ�ǧ��ڡʾ�̡ˤξ������̷׻�
           03  SYS-2010-NINJYOKBN           PIC  X(01).
           03  SYS-2010-NINJYO-TAIKBN       PIC  X(01).
      *Ĺ��ʻ�ѻ������̷׻��ʳ����
           03  SYS-2010-CYOKEIKBN-G         PIC  X(01).
           03  SYS-2010-CYOKEI-TAIKBN-G     PIC  X(01).
      *������̤����ʪ���չ��������绻�׻�
           03  SYS-2010-KOGAKKEIKBN         PIC  X(01).
           03  SYS-2010-KOGAKKEI-TAIKBN     PIC  X(01).
      *
      *����ۤ����������ô��׻�
           03  SYS-2010-KINJYOKBN-G         PIC  X(01).
           03  SYS-2010-KINJYO-TAIKBN-G     PIC  X(01).
           03  SYS-2010-KINJYOKBN-N         PIC  X(01).
           03  SYS-2010-KINJYO-TAIKBN-N     PIC  X(01).
      *��ݱ���ݾ�³ۻ��Ⱦ��
           03  SYS-2010-INNAIKBN            PIC  X(01).
           03  SYS-2010-INNAI-TAIKBN        PIC  X(01).
      *
      *û���ں߼���������������������̤���ô��׻�
           03  SYS-2010-TANKIKBN            PIC  X(01).
           03  SYS-2010-TANKI-TAIKBN        PIC  X(01).
      *
      *��ô��̤ȯ�����β��������ȡ�������
           03  SYS-2010-KAICNTKBN-N         PIC  X(01).
           03  SYS-2010-KAICNT-TAIKBN-N     PIC  X(01).
      *Ĺ��ʻ�ѻ������̷׻���������
           03  SYS-2010-CYOKEIKBN-N         PIC  X(01).
           03  SYS-2010-CYOKEI-TAIKBN-N     PIC  X(01).
      *�������ʻ�ѻ���������ô��׻���������
           03  SYS-2010-ZENDAYKBN-N         PIC  X(01).
           03  SYS-2010-ZENDAY-TAIKBN-N     PIC  X(01).
      *
      *��Ω�ٱ���Ť����̷׻�
           03  SYS-2010-JIRITUKBN           PIC  X(01).
           03  SYS-2010-JIRITU-TAIKBN       PIC  X(01).
      *
      *�������ʻ�ѻ������̷׻����ʳ����
           03  SYS-2010-ZENKEI2KBN-G        PIC  X(01).
           03  SYS-2010-ZENKEI2-TAIKBN-G    PIC  X(01).
      *
      *�������ʻ�ѻ���������ô��׻�����������
           03  SYS-2010-ZENDAY2KBN-N        PIC  X(01).
           03  SYS-2010-ZENDAY2-TAIKBN-N    PIC  X(01).
      *
      *��ǻ�����ô��׻��ʳ����
           03  SYS-2010-SYOKEI2KBN-G        PIC  X(01).
           03  SYS-2010-SYOKEI2-TAIKBN-G    PIC  X(01).
      *��ǻ�����ô��׻���������
           03  SYS-2010-SYOKEI2KBN-N        PIC  X(01).
           03  SYS-2010-SYOKEI2-TAIKBN-N    PIC  X(01).
      *
      *���ٳ�ǧ����󼨤�̵���������̷׻��ʹ���ԡ�
           03  SYS-2010-NINNASIKBN-K        PIC  X(01).
           03  SYS-2010-NINNASI-TAIKBN-K    PIC  X(01).
      *
      *������ô�⵭�ܣ���̤��ü�������ʳ����
           03  SYS-2010-FTNHASUKBN-G        PIC  X(01).
           03  SYS-2010-FTNHASU-TAIKBN-G    PIC  X(01).
      *������ô�⵭�ܣ���̤��ü��������������
           03  SYS-2010-FTNHASUKBN-N        PIC  X(01).
           03  SYS-2010-FTNHASU-TAIKBN-N    PIC  X(01).
      *
      *�ޥ�Ĺ��ô��׻�������ʬ
           03  SYS-2010-CYOKI-FTNKBN        PIC  X(01).
           03  SYS-2010-CYOKI-FTNKBN-TAIKBN PIC  X(01).
      *�ޥ�Ĺ�쥻�ץȵ��ܽ�����ʬ
           03  SYS-2010-CYOKI-KSIKBN        PIC  X(01).
           03  SYS-2010-CYOKI-KSIKBN-TAIKBN PIC  X(01).
      *
      *ͽ��
           03  SYS-2010-YOBI                PIC  X(318).
      *ü���ɣ�
           03  SYS-2010-TERMID              PIC  X(16).
      *���ڥ졼���ɣ�
           03  SYS-2010-OPID                PIC  X(16).
      *����ǯ����
           03  SYS-2010-CREYMD              PIC  X(08).
      *����ǯ����
           03  SYS-2010-UPYMD               PIC  X(08).
      *��������
           03  SYS-2010-UPHMS               PIC  X(06).
      *�祭�������ѹ���
           03  SYS-2010-UPD-KEY.
               05  SYS-2010-UPD-HOSPNUM     PIC 9(2).
               05  SYS-2010-UPD-KANRICD     PIC X(04).
               05  SYS-2010-UPD-KBNCD       PIC X(08).
               05  SYS-2010-UPD-STYUKYMD    PIC X(08).
               05  SYS-2010-UPD-EDYUKYMD    PIC X(08).