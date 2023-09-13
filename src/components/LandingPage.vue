<template>
    <body>
        <div class='container'>
            <div class='top'>
                <div class='logo'>
                    <span>
                        psf
                    </span>
                </div>
            </div>
            <div class='middle'>
                <h1 class='title'>
                    Recreational quantitative trading 🍊
                </h1>
                <br/>
                <h2 class='subtitle'>
                    Pocket Size Fund is a quantitative proprietary trading firm being built on nights and weekends
                </h2>
                <br/>   
                <h2 class='call-to-action'>
                    Follow our progress and performance on <a 
                        href='https://twitter.com/pocketsizefund' 
                        target='_blank'
                    >Twitter</a>
                </h2>
                <br/>
                <br/>
                <!-- <div class='performance'>
                    <h2>Performance</h2>
                    <br/>
                    <table v-if='errorMessage === ""'>
                    </table>
                    <span v-if='errorMessage !== ""'>
                        {{ errorMessage }}
                    </span>
                </div> -->
            </div>
        </div>
    </body>
</template>

<script>
import axios from 'axios';

export default {
    name: 'LandingPage',
    data: function() {
        return {
            errorMessage: '',
        };
    },
    created: function () {
        const url = process.env.VUE_APP_GET_PERFORMANCE_API_ENDPOINT;

        const config = {
            headers: {
                'x-pocket-size-fund-api-key': process.env.VUE_APP_API_KEY,
            }
        }

        axios.get(url, config)
            .then((/*response*/) => {
                // TODO: implement after backend is complete
            })
            .catch(() => {
                this.$data.errorMessage = 'Data is not available yet.';
            }
        );
    },
};
</script>

<style>
body {
    font-family: 'Lexend', sans-serif;
    background-image: url('../assets/background-oranges.jpg');
    background-repeat: no-repeat;
    background-size: cover;
    background-attachment: fixed;
    height: 100%;
    width: 100%;
    color: #ffffff;
}

.container {
    height: 100%;
    margin: auto;
    width: 34rem;
    padding: 2rem 1rem 0rem;

}

.top > div {
    display: inline-block;
}

.logo {
    font-size: 3rem;
    font-weight: 800;
}

a {
    color: #ffffff;
}

.middle {
    padding-top: 6rem;
}

.title {
    font-size: 4rem;
    font-weight: 800;
    align-content: center;
}

.subtitle,
.call-to-action {
    font-size: 2rem;
    font-weight: 800;
    align-content: center;
}

.performance {
    padding-top: 2rem;
    font-size: 1.5rem;
}

@media screen and (max-width: 580px) {
    .container {
        width: 100%;
    }

    .middle {
        padding-top: 2rem;
    }

    .title {
        font-size: 2.5rem;
    }

    .subtitle,
    .call-to-action {
        font-size: 1.5rem;
    }
}
</style>