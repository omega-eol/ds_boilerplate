# return basic validation metrics for Supervised models
validate = function(predicted, groundtruth, class_names = NULL, 
                    th = NULL, n_points = 100, 
                    verbose=TRUE, debug=FALSE, plot_graph=TRUE, compute_auc_pr=TRUE,
                    filename=NULL) {
     
    # GROUNDTRUTH
    # check the length groundtruth vector/factor
    n = length(groundtruth);
    # convert groundtruth to a factor if needed
    if (!is.factor(groundtruth)) {
        groundtruth_f = as.factor(groundtruth);
        groundtruth_v = groundtruth;
        if (debug) message('Converted vector of groundtruth to a factor');
    } else {
        groundtruth_f = groundtruth;
    };
    # find classes and its number
    classes = levels(groundtruth_f);
    k = nlevels(groundtruth_f);
    if (debug) message("GROUNDTRUTH: ", n, " samples, ", k, " classes: [", paste(classes, collade=",", sep=""), "]");
    
    # PREDICTED
    binary_prob = FALSE;
    if (is.numeric(predicted)) {
        # if we are here, then predicted is not a factor
        if (is.matrix(predicted)) {
            # predicted is a matrix
            if (debug) message("Predicted is a matrix. We assume that each column is a class.");
            if (is.null(colnames(predicted))) colnames(predicted) = 1:ncol(predicted);
            predicted_v = apply(predicted, 1, function(x) colnames(predicted)[which.max(x)] );
            predicted_f = as.factor(predicted_v);
        } else {
            # if we are here, then predicted is a vector. What kind of vector?
            if (all(predicted == floor(predicted))) {
                # all elements of the predicted vector are integers
                predicted_v = predicted;
                predicted_f = as.factor(predicted);
            } else {
                # if we are here, then this is a binary case and we have
                # a vector of probabilities, where each element is a probability
                # of a point being in class "1"
                
                # check if we have a binary problem
                if (k != 2) stop('Predicted is a vector of real numbers - vector of...
                                 of probabilities but at the same time number of...
                                 detected classes is not 2!');
                
                # do a prediction with a default threshold for now
                predicted_f = rep(0, length(predicted));
                predicted_f[predicted>=0.5] = 1;
                predicted_f = as.factor(predicted_f);
                
                # mark the fact predicted is a vector of probabilities
                predicted_v = predicted;
                binary_prob = TRUE;
            };
        }
    } else {
        # if we are here, then predictor is factor (probably)
        if (is.factor(predicted)) {
            # its a factor
            predicted_f = predicted;
        } else {
            stop('Predicted is of unknow type');
        }
    };
    # number of points predicted
    n_predicted = length(predicted_f);
    # classes predicted
    predicted_classes = levels(predicted_f);
    k_predicted = nlevels(predicted_f);
    if (debug) {
        message("PREDICTED: ", n_predicted, " samples, ", k_predicted, 
                " classes: [", paste(predicted_classes, collade=",", sep=""), "]");
    };
    
    # ERROR CHECK
    # length check
    if (n != n_predicted) stop("Number of points in predicted is different than in groundtruth.");
    
    # deal with the binary prob case
    bin_out = NULL;
    if (binary_prob) {
        bin_out = binary_valid(predicted_v, groundtruth_f,
                               th=th, n_points=n_points,
                               plot_graph=plot_graph, compute_auc_pr=compute_auc_pr);
        predicted_f = bin_out$predicted_f;
    };
    
    # compute confusion matrix
    tt = table(predicted_f, groundtruth_f);
    
    # find number of classes on actual data
    missing_classes = setdiff(union(predicted_classes, classes), intersect(predicted_classes, classes));
    if (k != k_predicted | length(missing_classes) > 0) {
        warning("Number of predicted classes is different than number of classes in groundtruth data.");
        message('Different classes detected:'); print(missing_classes);
        
        # this makes everything a little bit complicated
        merged_classes = sort(union(predicted_classes, classes));
        k = length(merged_classes);
        
        # variables
        accuracy = rep(0, k); precision = rep(0, k); recall = rep(0, k);
        groundtruth_dist = rep(0, k); predicted_dist = rep(0, k);
        for (i in 1:k) {
           class_name = merged_classes[i];
           row_index = which(predicted_classes==class_name);
           col_index = which(classes==class_name);
           
           predicted_dist[i] = sum(predicted_f==class_name);
           groundtruth_dist[i] = sum(groundtruth_f==class_name);
           
           if (length(row_index) > 0 && length(col_index) > 0) {               
                # measures
                tp = tt[row_index, col_index];
                fp = sum(tt[row_index,])-tp;
                fn = sum(tt[,col_index])-tp;
                tn = n-tp-fp-fn;
                
                # metrics
                accuracy[i] = (tp+tn)/n;
                precision[i] = tp/(tp+fp);
                recall[i] = tp/(tp+fn);
           };
        
        };
          
    } else { # this is normal case, when classes are match
      
        # variables
        accuracy = rep(0, k); precision = rep(0, k); recall = rep(0, k);
        groundtruth_dist = colSums(tt);
        predicted_dist = rowSums(tt);
        for (i in 1:k) {
           # measures
           tp = tt[i,i];
           fp = sum(tt[i,])-tp;
           fn = sum(tt[,i])-tp;
           tn = n-tp-fp-fn;
           
           # metrics
           accuracy[i] = (tp+tn)/n;
           precision[i] = tp/(tp+fp);
           recall[i] = tp/(tp+fn);
        };    
      
    }; # end if
    
    # find F-measure
    f_measure = 2*precision*recall/(precision+recall);
    # replace NaNs with 0 if needed
    f_measure[is.nan(f_measure)] = 0;
    
    # compute weighted F-measure
    w_f_measure = sum(groundtruth_dist*f_measure/n);
    
    # prepare the output
    df = data.frame(classes, accuracy, precision, recall, f_measure, 
                    predicted_dist/n, groundtruth_dist/n, predicted_dist, groundtruth_dist,
                    (groundtruth_dist-predicted_dist)/groundtruth_dist, 
                    precision/(groundtruth_dist/n) 
                    );
    df_names = c('Class', 'Accuracy', 'Precision', 'Recall', 'F-measure', 
                  'Predicted Distribution', 'Groundtruth Distribution', '# Predicted', '# Groundtruth', 
                  'Distribution Delta', 'Gain'
                  );
    names(df) = df_names;
    rownames(df) = NULL;
    
    # add binary metrics
    if (binary_prob) {
      df = cbind(df, rep(bin_out$auc, k), rep(bin_out$auc_pr$auc.davis.goadrich, k)); 
      names(df) = c(df_names, 'AUC', 'AUC-PR');
    };
    
    # if class_name vector is not null replace class names in Class column in final data frame
    if (!is.null(class_names)) {
        if (debug) message('Renaming class names..');
        f = as.numeric(levels(df$Class))[df$Class];
        if (f[1]==0) f = f+1; # if class starts from 0, add +1
        df$Class = class_names[f];
        df = df[order(df$Class),]
    };
    
    # prepare output list
    res = list("df" = df, "avg_f_measure" = mean(f_measure), "w_f_measure" = w_f_measure, 
            "auc" = bin_out$auc, "auc_pr" = bin_out$auc_pr, 
            "sensitivity" = bin_out$sensitivity, "specificity" = bin_out$specificity, 
            "th" = bin_out$th);
    
    # output
    if (verbose) {
        message('Validation is done base on ', n, ' samples:');
        print(res$df);
        message('Average F-measure: ', res$avg_f_measure, ' (weigthed: ',res$w_f_measure, ')');
        message('Average Delta Distribution: ', mean(abs((groundtruth_dist-predicted_dist)/groundtruth_dist)), '.');
        if (binary_prob) {
            message('Decision threshold: ', bin_out$th, ".");
            message('AUC: ', bin_out$auc, ', AUC-PR: ', bin_out$auc_pr$auc.davis.goadrich);
        };
    };
    
    # save output as csv file
    if (!is.null(filename)) {
      if (basename(filename) == filename) filename = paste0(getwd(), "/", filename);
      message('Saving results in ' ,filename, '..');
      dir.create(dirname(filename), showWarnings = FALSE, recursive = TRUE);
      write.table(res$df, file=filename, row.names=FALSE, sep=",");
    };
    
    return(res);
};

# helper function for binary classification problems
binary_valid = function(predicted_v, groundtruth,
                        th=NULL, n_points=100,
                        plot_graph=TRUE, compute_auc_pr=TRUE) {
    # initialization
    n = length(predicted_v);
    auc = NULL; auc_pr = NULL;
    sensitivity = NULL; specificity = NULL;    
    
    # calculate AUC of Precision - Recall curve
    if (compute_auc_pr) {
        library(PRROC);
        y = groundtruth;
        if (is.factor(groundtruth)) y = as.numeric(levels(groundtruth))[groundtruth]
        auc_pr = tryCatch({
            pr.curve(scores.class0=predicted_v, weights.class0=y, curve=plot_graph);          
        }, error = function(err) {
            message(err);
            return(list(auc.davis.goadrich = 0));
        });
        if (plot_graph) plot(auc_pr);
    } else {
        auc_pr = list(auc.davis.goadrich = 0);
    };
    
    # calculate AUC 
    ranked_prediction = rank(predicted_v);
    index = groundtruth_f==1; np = sum(index);
    auc = (sum(ranked_prediction[index]) - np*(np+1)/2)/np/(n-np);
    
    # plot ROC: sensitivity = recall = true positive rate = TP/(TP+FN)
    # sort predicted probability
    sort_res = sort(predicted_v, index.return=TRUE);
    prob = predicted_v[sort_res$ix];
    ground = groundtruth_f[sort_res$ix];
    
    # number of points on the plot
    n_plot_points = min(n, n_points+2);
    
    # for eah point - threshold value
    position_index = floor(seq.int(from = 1, to = n, length.out = n_plot_points));
    sensitivity = rep(0, n_plot_points); specificity = rep(0, n_plot_points); predicted_np = rep(0, n_plot_points);
    for (i in 2:n_plot_points) {
        i_th = prob[position_index[i]];
        # everything > i_th is 1
        tp = sum(ground[position_index[i]:n]==1);
        fn = sum(ground[1:(position_index[i]-1)]==1);
        tn = sum(ground[1:(position_index[i]-1)]==0);
        fp = sum(ground[position_index[i]:n]==0);
        sensitivity[i] = tp/(tp+fn);
        specificity[i] = tn/(tn+fp);
        
        # number of predicted positive records
        predicted_np[i] = n-position_index[i]+1;
    };          
    # boundary conditions
    sensitivity[1] = 1;
    specificity[n_plot_points] = 1;
    
    # do actual plot
    if (plot_graph) {
        plot(1-specificity, sensitivity, type="l", col="red", xlim=c(0,1), ylim=c(0,1),
             main=paste0("ROC curve\nAUC = ", sprintf("%8.7f", auc)));
        lines(x=c(0, 1), y=c(0, 1), lty=2);
    };
    
    # find optimal threshold (approximately)
    if (is.null(th)) th = prob[position_index[which.min(abs(np/n - predicted_np/n))]];
    
    # convert to predictions
    prob = rep(0, n);
    prob[predicted_v>=th] = 1;
    predicted_f = as.factor(prob);
    
    bin_out = list("auc"=auc, "auc_pr"=auc_pr, 
                   "sensitivity"=sensitivity, "specificity"=sensitivity,
                   "th"=th, "predicted_f"=predicted_f);
    return(bin_out);
};

# finds accuracy for the top K
# mat - matrix of probabilities
# groundtruth - vector of ground-truth
# k - number of top values
topK = function(mat, groundtruth, k=5, plot_values=FALSE) {
     n = nrow(mat);     
     if (n != length(groundtruth)) stop('Probability matrix has different number of rows than the ground-truth vector.');
     
     k_stats = matrix(0, ncol=k, nrow=n);
     for (i in 1:n) {
          top_values = as.numeric(names(sort(mat[i,], decreasing = TRUE)))[1:k];
          k_stats[i,] = top_values == groundtruth[i];
     };
     
     # find top k
     top_k = rep(0, k);
     for (i in 1:k) top_k[i] = sum(k_stats[,(1:i)])/n;
     
     # plot values in needed
     if (plot_values) {
          plot(top_k, type='b', main=paste0('Accuracy for top ', k ,'.'),
               xlab='k', ylab='Accuracy');
     };
     
     return(top_k);
};