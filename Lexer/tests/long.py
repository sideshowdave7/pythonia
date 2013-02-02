import Orange
import nltk
import io, os 
import datastore
import pickle

stemmer = nltk.stem.LancasterStemmer()
dt = datastore.DataStore()
stopwords = nltk.corpus.stopwords.words('english') +\
            ['thing']+\
            ['something']+\
            ['everything']+\
            ['anything']+\
            ['problem']+\
            ['problems']+\
            ['way']

def nn_transaction(product, filename):
    
    outf = open (filename, 'w')
    stem_dict = {}

    for product in products: 
        for rid in dt.get_review_ids(product):
            review = dt.get_product_review(product,rid)

            for sentence in review['sentences']:
    ##                print sentence['words']

                stems = ''
                words = sentence['words']
                NN = [(i,  word['text']) for i,word in zip(xrange(0,len(words)), words) \
                                                    if word['pos'].startswith('NN') \
                                                        and len(word['text']) > 1 \
                                                        and word['text'] not in stopwords]
    ##                print NN
    ##                outf.write("%s" % NN)
    ##                outf.write(' => ')

                # write out first NN to transaction
                if NN:
                    stems += NN[0][1]#stemmer.stem(NN[0][1])
                    outf.write(stems)

                for i in xrange(1, len(NN)):
                    prei = NN[i-1][0]
                    curri = NN[i][0]

    ##                    print prei, curri
                    
                    if (curri - prei) == 1 and len(stems.split()) < 3:
                        stems += ' '
                        stem   = NN[i][1]#stemmer.stem(NN[i][1])
                        stems += stem
                        outf.write(', ')
                        outf.write(stem)
                        outf.write(', ')
                        outf.write(stems)


                    else:
                        stems = ''
                        stems += NN[i][1] #stemmer.stem(NN[i][1])
                        stem_dict[stems] = NN[0][1]
                        outf.write(', ')    
                        outf.write(stems)


                if NN:
                    outf.write('\n') # insert newline for next transaction

##    pickle.dump(stem_dict, open('stems_%d_.p'%product, 'wb'))
    
##                break
##            break
                
##                while i < len(words):
##                    tx,pos = words[i]['text'],words[i]['pos']
##                    if tx not in stopwords and pos.startswith('NN') \
##                        and len(tx) > 1:
##                        stems += stemmer.stem(tx)
##                        outf.write(stems)
##
##                
##                while i < len(words):
##                    
##                    tx,pos = words[i]['text'],words[i]['pos']
##                    if tx not in stopwords and pos.startswith('NN') \
##                        and len(tx) > 1:
##                        stems += stemmer.stem(tx)
##                        outf.write(stems)
##                        outf.write(', ')
##                        
##                        tx,pos = words[i]['text'],words[i]['pos']
##                        if tx not in stopwords and pos.startswith('NN') \
##                            and len(tx) > 1:
##                            stems += ' '
##                            stems += stemmer.stem(tx)
##                            i += 1
##                            tx,pos = words[i+1]['text'],words[i+1]['pos']
##                            if tx not in stopwords and pos.startswith('NN') \
##                                and len(tx) > 1:
##                                stems += ' '
##                                stems += stemmer.stem(tx)
##                                i += 1
##                        
##                    if stems:
##                        outf.write(stems)
##                        outf.write(', ')
##                        stems = ''
##                        
##                outf.write('\n')
            
##def feature_transaction(filename):
##
##    f = open(filename, 'r')
####    wf = open ('nn_transaction.basket', 'w')
##    for line in f.readlines():
##        s = line.split('##')
##        if len(s) ==1:
##            tokens = nltk.word_tokenize(s[0].lower())
##            
##            print [t for t in tokens if t not in nltk.corpus.stopwords.words('english') or len(t)==1]
##            nn = [n for n in nltk.pos_tag(tokens) if n[1].startswith('NN')]
##            print nn    
##        else:
##            tokens = nltk.word_tokenize(s[1].lower())
##            nn = [n for n in nltk.pos_tag(tokens) if n[1].startswith('NN')]
##            print nn
##        for n in nn:
##            wf.write(n[0])
##            wf.write(', ')
##            
##        wf.write('\n')
    

##feature_transaction('../toydata/simple_5_product_reviews/Apex AD2600 Progressive-scan DVD player.txt')

def frequent_itemsets(filename):
    
    data = Orange.data.Table(filename)

    ind = Orange.associate.AssociationRulesSparseInducer(support=0.01)#, storeExamples = True)
    itemsets = ind.get_itemsets(data)
##    for itemset, tids in itemsets:
##        print "(%4.2f) %s" % (len(tids)/float(len(data)),
##                          " ".join(data.domain[item].name for item in itemset))
    freq_itemsets = [" ".join(data.domain[item].name for item in itemset) for itemset, tids in itemsets] 
    return freq_itemsets


def find_index(i, words, stem):

    for k in xrange(i, len(words)):
        if words[k]['text'] == stem: #stemmer.stem(words[k]['text']) == stem:
            return k

    return -1
        
def compact_pruning(itemsets, products, dist_boundary=3, sent_count = 3):

    
    count_table = {}
    
    for f in itemsets:
        words = f.split() 
        if len(words) > 1: # compact prune aspects with more than 1 word only
            count_table[f] = 0

    for product_id in products:
        
        for rid in dt.get_review_ids(product_id):
            review = dt.get_product_review(product_id,rid)           
        
            for sentence in review['sentences']:

                words = sentence['words']

                for f in count_table:
                    stems = f.split()

                    # search for matching word stem
                    if len(stems) == 2:
                        
                        start = find_index(0,words, stems[0]) 
                        end = find_index(start+1,words, stems[1]) 
    ##                    i = 0
    ##                    for word in sentence['words']:
    ##                        if word['pos'].startswith('NN'):
    ##                            if stemmer.stem(word['text']) == stems[0]:
    ##                                start = i    
    ##                            elif stemmer.stem(word['text']) == stems[1]:
    ##                                end = i
    ##                        i+= 1
                                    
                        if start > -1 and end > -1: 
                            dist = abs(end-start)-1
                            if dist <= dist_boundary:
                                count_table[f] += 1
                                print 'feature: ', f, 'context: ', sentence['text']

                    elif len(stems) == 3: 
                        start = find_index(0,words, stems[0]) 
                        mid = find_index(start+1,words, stems[1]) 
                        end = find_index(mid+1,words, stems[2]) 

    ##                    i = 0
    ##                    for word in sentence['words']:
    ##                        if word['pos'].startswith('NN'):
    ##                            if stemmer.stem(word['text']) == stems[0]:
    ##                                start = i
    ##                            elif stemmer.stem(word['text']) == stems[1]:
    ##                                mid = i    
    ##                            elif stemmer.stem(word['text']) == stems[2]:
    ##                                end = i
    ##                        i+= 1
                                    
                        if start > -1 and mid > -1 and end > -1: 
                            dist1 = abs(end-mid)-1
                            dist2 = abs(mid-start)-1
                            if dist1 <= dist_boundary and dist2 <= dist_boundary:
                                count_table[f] += 1
                                print 'feature: ', f, 'context: ', sentence['text']
                            
    print count_table            

    for f,c in count_table.iteritems():
        if c < sent_count:
            itemsets.remove(f)

##    ranked_features = sorted(count_table.items(),key=lambda x: x[1], reverse=True)
##    print ranked_features

##    for item in itemsets:
##        words = item.split()
##        for f,c in count_table.items():
##            fwords = f.split()
##            if set(words).intersection(set(fwords)):
##                if count_table[item] < 
        
    return itemsets


def psupport_pruning( itemsets, products, min_psupport = 3 ):

    count_table = {}

    for f in itemsets:
        
        stems = f.split()
        superset = [ss for ss in itemsets if len(ss.split()) > 1 and f in ss.split()]
        
        if len(stems) == 1 and superset: # prune aspects with one word only
            count_table[f] = {}
            count_table[f]['count'] = 0
            count_table[f]['superset'] =[]
            for w in superset:
                count_table[f]['superset'] += [a for a in w.split() if a != f]

    for product_id in products:
        
        for rid in dt.get_review_ids(product_id):
                review = dt.get_product_review(product_id,rid)           
            
                for sentence in review['sentences']:            

                    words = sentence['words']

                    for f in count_table:

                        if find_index (0,words,f) > -1:
                            # check if any superset in sentence also
                            match = [w for w in count_table[f]['superset'] if find_index (0,words,w) > -1]
                            if not match:
                                count_table[f]['count'] += 1
                    

    print count_table
    
    for f in count_table:
        if count_table[f]['count'] < min_psupport: #prune
            itemsets.remove(f)
            

    return itemsets
    
def get_aspects():
    return aspects
    
##def extract():
if __name__ == '__main__':

    aspects = []#{}
    products = dt.get_product_ids()
##    for product in products:
##        name = dt.get_product_name(product)
##        
##        if not os.path.isfile('NN_transaction_'+name+'.basket'):
##            print "aspect_extraction: Building NN transaction file ..."
##            print "aspect_extraction: product: %s -> 'NN_transaction_%s.basket" % (name,name)
##            nn_transaction(product,'NN_transaction_%s.basket'%name)

    nn_transaction(products,'NN_transaction_TVs.basket')        
    print "aspect_extraction: retrieving aspects from transaction ..." 
    itemsets = frequent_itemsets('NN_transaction_TVs.basket')
    print "aspect_extraction: prunning aspects ..."
    itemsets = compact_pruning(itemsets, products)
    itemsets = psupport_pruning(itemsets, products)
    aspects = itemsets #[product] = itemsets

    pfile = open ('aspects.p', 'wb')
    pickle.dump(aspects, pfile, pickle.HIGHEST_PROTOCOL)
    
    

##extract()
##main()

